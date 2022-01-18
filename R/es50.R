#' Compute ES50 for a sf table of MPAs
#' 
#' @export
#' @param mpa sf table of MPAs, possibly with a column of 'obs'.  If that column
#'   is missing we add it by calling \code{fetch_obis} which could take some time.
#' @param verbose logical, output helpful messages
#' @return the same mpa row with 'total_records', 'total_phyla', 'total_species', 
#'   'es50_overall' and possibly 'obs' if it doesn't already exist
mpa_es50 <- function(mpa, verbose = interactive()){
  
  if (!inherits(mpa, "sf")) stop("mpa input must be of 'sf' class")
  
  if (!("obs" %in% colnames(mpa))){
    if (verbose) cat("fetching OBIS observation data\n")
    obs <- fetch_obis(mpa, form = "sf")
    mpa <- mpa_knit_obs(mpa, obs)
  }
  
  mpa |>
    dplyr::group_by(.data$WDPAID) |>
    dplyr::group_map(es50_base, .keep = TRUE, verbose = verbose) |>
    dplyr::bind_rows()
}


#' Compute ES50 at timesteps for a sf table of MPAs
#' 
#' @export
#' @param mpa sf table of MPAs, possibly with a column of 'obs'.  If that column
#'   is missing we add it by calling \code{fetch_obis} which could take some time.
#' @param ages numeric, age(s) to compute es50 for
#' @param step_size numeric of years in each step of analysis
#' @param verbose logical, output helpful messages
#' @return the same mpa row with 'diversity' column, and possibly 'obs' if it doesn't already exist
mpa_timeblock <- function(mpa = read_mpa("Cuba"), 
                          ages = c(10, 20), 
                          step_size = 10, 
                          verbose = interactive()){
  
  if (!inherits(mpa, "sf")) stop("mpa input must be of 'sf' class")
  
  if (!("obs" %in% colnames(mpa))){
    if (verbose) cat("fetching OBIS observation data\n")
    obs <- fetch_obis(mpa, form = "sf")
    mpa <- mpa_knit_obs(mpa, obs)
  }
  
  
  
  es <- lapply(seq_len(nrow(mpa)),
               function(i){
                 es_intervals(mpa |> dplyr::slice(i),
                              ages = ages,
                              step_size = step_size,
                              verbose = verbose)
               })
  
  geom_ix <- which_geometry(mpa)
  mpa |>
    dplyr::mutate(diversity = es, .before = geom_ix)
}



#' Calculate es50 metric for an mpa for specific time windows
#'
#' @param mpa a single row from wdpa table containing one mpa with metadata, obs and geometry column
#'   If the obs column is not present then it is fetched from OBIS
#' @param key the wdpaid of the mpa (ignored)
#' @param ages numeric, one or more 'ages' relative to 'STATUS_YR' to compute es50 for
#' @param step_size numeric duration (in years) for each age (interval = age + time_step)
#' @param sample_size numeric see \code{\link[vegan]{rarefy}} \code{sample} argument.  50 computes ES50.
#' @param verbose logical, if TRUE output helpful messages
#' @return tibble of ES statistics
#' @export
es_intervals <- function(mpa, key, 
                         ages = c(10, 20), # make plural
                         step_size = 10, 
                         sample_size = 50,
                         verbose = interactive()) {
  
  if (nrow(mpa) > 1) stop("one row at a time please")
  
  samp <- as.numeric(sample_size[1])
  if (verbose) cat(sprintf("es%i_intervals WDPAID: %s", samp[1], mpa$WDPAID[1]), "\n")
  
  if (!("obs" %in% colnames(mpa))){
    if (verbose) cat("fetching OBIS observations...\n")
    # request the records based upon geometry column
    records <- tryCatch(
      {
        #robis::occurrence(geometry = sf::st_as_text(sf::st_convex_hull(mpa[[geom_ix[1]]])))
        fetch_obis(mpa, form = 'sf')
      },
      error = function(e){
        message('Error in es50_timeblock')
        print(e)
        return(NULL)
      })
    mpa <- mpa_knit_obs(mpa, records)
  }
  
  interval_one <- function(mpa, 
                           age = 10, 
                           step_size = 10, 
                           samp = 50, 
                           verbose = interactive()){
  
    # can't find ES50 in the future, so return
    if ((mpa$STATUS_YR + age) > as.numeric(format(Sys.Date(),format="%Y"))) {
      if (verbose) cat(paste(mpa$WDPAID, "MPA is too young, age:", age, "\n", sep=" "))
      
      #geom_ix <- which_geometry(mpa)
      
      #mpa <- mpa %>%
      #  dplyr::mutate("age_{age}" := NA, .before = geom_ix)
      r <- dplyr::tibble(
        WDPAID                = mpa$WDPAID,
        age                   = age,
        "es{samp}_age"       := NA_real_,
        "species_age"        := NA_real_,
        "phylum_age"         := NA_real_,
        "records_age"        := NA_real_)
      return(r)
    }
    
    #filter occurrence records for bin calculation
    #if (age == 0) {
    #  #maybe (step_+size - 1)
    #  age_year <- mpa$STATUS_YR + step_size[1] - 1
    #  
    #  age_records <- mpa$obs[[1]] %>%
    #    dplyr::filter(dplyr::between(.data$date_year, mpa$STATUS_YR, age_year))
    #  
    #} else {
      bin_start <- mpa$STATUS_YR + age
      
      bin_end <- bin_start + step_size[1] - 1
      
      age_records <- mpa$obs[[1]] %>%
        dplyr::filter(dplyr::between(.data$date_year, bin_start, bin_end))
    #}
    
    #compute ES{samp} for filtered occurrence records by bin
    if (nrow(age_records) > 0) {
      # there is no "Count" field
      species_counts <- aggregate(as.integer(age_records$individualCount),
                                  by=list(Category=age_records$scientificName),
                                  FUN=sum)
      
      phylum_counts <- aggregate(as.integer(age_records$individualCount),
                                 by=list(Category=age_records$phylum),
                                 FUN=sum)
      
      if (nrow(species_counts) >= samp[1]) {
        
        if (verbose) cat(sprintf("%s calculating es%i, age: %i", mpa$WDPAID, samp[1], age[1]),"\n")
        
        es <- vegan::rarefy(as.integer(species_counts$x), samp[1])
        
        
        r <- dplyr::tibble(
          WDPAID                = mpa$WDPAID,
          age                   = age,
          "es{samp}_age"       := es,
          "species_age"        := nrow(phylum_counts),
          "phylum_age"         := nrow(phylum_counts),
          "records_age"        := sum(species_counts$x))
      } else {
        if (verbose) cat(paste(mpa$WDPAID, "Fewer than 50 species records, age:", age, "\n", sep=" "))
        
        r <- dplyr::tibble(
          WDPAID                = mpa$WDPAID,
          age                   = age,
          "es{samp}_age"       := NA_real_,
          "species_age"        := NA_real_,
          "phylum_age"         := NA_real_,
          "records_age"        := NA_real_)
      }
    } else {
      if (verbose) cat(paste(mpa$WDPAID, "No records, age:", age, "\n", sep=" "))
      r <- dplyr::tibble(
        WDPAID                = mpa$WDPAID,
        age                   = age,
        "es{samp}_age"       := NA_real_,
        "species_age"        := NA_real_,
        "phylum_age"         := NA_real_,
        "records_age"        := NA_real_)
    }
    
    return(r)
  } # interval_one
  
  
  lapply(ages,
         function(age){
           interval_one(mpa, age = age, step_size = step_size, 
                        samp = samp, verbose = verbose)
         }) |>
    dplyr::bind_rows()
}


#' Record number of records and attempt an overall es50 calculation
#'
#' @seealso \code{\link{mpa_es50}}
#' @export
#' @param mpa a single row of an mpa records table
#' @param key the wdpaid of the mpa
#' @param verbose logical, if TRUE output helpful messages
#' @return the same mpa row with total_records, total_phyla, total_species, es50_overall
#'  or NULL if an error occurs
es50_base <- function(mpa, key, verbose = interactive()) {
  
  if (verbose) cat("es50_base WDPAID: ", mpa$WDPAID[1], "\n")
  
  # which column is likely to contain geometry?
  geom_ix <- which_geometry(mpa)
  
  if (!("obs" %in% colnames(mpa))){
    if (verbose) cat("fetching OBIS observations...\n")
    # request the records based upon geometry column
    records <- tryCatch(
      {
        #robis::occurrence(geometry = sf::st_as_text(sf::st_convex_hull(mpa[[geom_ix[1]]])))
        fetch_obis(mpa, form = 'sf')
      },
      error = function(e){
        message('Error in es50_base')
        print(e)
        return(NULL)
      })
    mpa <- mpa_knit_obs(mpa, records)
  } else {
    records <- mpa$obs[[1]]
  }
  
  if (!rlang::is_empty(records) && nrow(records) > 0) {

    if ("individualCount" %in% colnames(records)) {

      records$individualCount <- as.numeric(records$individualCount)
      records$individualCount[is.na(records$individualCount)] <- 1
      records$Count <- 1 * records$individualCount

    } else {
      records$Count <- 1
    }

    species_count <- aggregate(records$Count,
                               by=list(Category=records$scientificName),
                               FUN=sum)

    phylum_count <- aggregate(records$Count,
                              by=list(Category=records$phylum),
                              FUN=sum)

    mpa <- mpa %>%
      dplyr::mutate(species_count_all = nrow(species_count),
                    phylum_count_all = nrow(phylum_count),
                    records_all = sum(species_count$x),
                    .before = geom_ix)

    if (nrow(species_count) >= 50){
      total_es50 = vegan::rarefy(as.integer(species_count$x),50)
      geom_ix <- which_geometry(mpa)
      mpa <- mpa %>%
        dplyr::mutate(total_es50 = total_es50,
                      .before = geom_ix)
    }

  } else {
    mpa <- mpa %>%
      dplyr::mutate(total_es50 = NA_real_,
                    species_count_all = 0,
                    phylum_count_all = 0,
                    records_all = 0,
                    .before = geom_ix)
  }
  
  #if (verbose) cat("\n")
  
  return(mpa)
}


#' Calculate es50 metric for an mpa for specific time points
#'
#' @param mpa a row from wdpa table containing one mpa with metadata, obs and geometry column
#'   If the obs column is not present then it is fetched from OBIS
#' @param key the wdpaid of the mpa
#' @param age numeric, ages to compute es50 for
#' @param step_size numeric duration in each step of analysis
#' @param verbose logical, if TRUE output helpful messages
#' @return single row table containing same columns as mpa but with added timeblock columns
#'
#' @export
es50_timeblock <- function(mpa, key, 
                           age = 10, # make plural
                           step_size = 10, 
                           verbose = interactive()) {
  if (verbose) cat("es50_base WDPAID: ", mpa$WDPAID[1], "\n")
  
  if (!("obs" %in% colnames(mpa))){
    if (verbose) cat("fetching OBIS observations...\n")
    # request the records based upon geometry column
    records <- tryCatch(
      {
        #robis::occurrence(geometry = sf::st_as_text(sf::st_convex_hull(mpa[[geom_ix[1]]])))
        fetch_obis(mpa, form = 'sf')
      },
      error = function(e){
        message('Error in es50_timeblock')
        print(e)
        return(NULL)
      })
    mpa <- mpa_knit_obs(mpa, records)
  }
  
  # can't find ES50 in the future
  if ((mpa$STATUS_YR + age) > as.numeric(format(Sys.Date(),format="%Y"))) {
    if (verbose) cat(paste(mpa$WDPAID, "MPA is too young, age:", age, "\n", sep=" "))

    geom_ix <- which_geometry(mpa)
    
    mpa <- mpa %>%
      dplyr::mutate("age_{age}" := NA, .before = geom_ix)

    return(mpa)
  }

  #filter occurrence records for bin calculation
  if (age == 0) {
    # why 19?  maybe (step_+size - 1)
    age_year <- mpa$STATUS_YR + step_size[1] - 1 # 19
    
    age_records <- mpa$obs[[1]] %>%
      dplyr::filter(dplyr::between(.data$date_year, mpa$STATUS_YR, age_year))

  } else {
    bin_start <- mpa$STATUS_YR + age

    bin_end <- bin_start + step_size[1] - 1 # 19

    age_records <- mpa$obs[[1]] %>%
      dplyr::filter(dplyr::between(.data$date_year, bin_start, bin_end))
  }

  #compute es50 for filtered occurrence records by bin
  if (nrow(age_records) > 0) {
    # there is no "Count" field
    species_counts <- aggregate(age_records$Count,
                                by=list(Category=age_records$scientificName),
                                FUN=sum)

    phylum_counts <- aggregate(age_records$Count,
                               by=list(Category=age_records$phylum),
                               FUN=sum)

    if (nrow(species_counts) >= 50) {
      
      if (verbose) cat(paste(mpa$WDPAID, "calculating es50, age:", age, "\n", sep=" "))

      es_50 <- vegan::rarefy(as.integer(species_counts$x), 50)

      geom_ix <- which_geometry(mpa)
      
      # see https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/
      mpa <- mpa %>%
        dplyr::mutate("es50_age_{age}"    := es_50,
                      "species_age_{age}" := nrow(species_counts),
                      "phylum_age_{age}"  := nrow(phylum_counts),
                      "records_age_{age}" := sum(species_counts$x),
                      .before = geom_ix)

      return(mpa)
    } else {
      if (verbose) cat(paste(mpa$WDPAID, "Fewer than 50 species records, age:", age, "\n", sep=" "))

      geom_ix <- which_geometry(mpa)
      
      mpa <- mpa %>%
        dplyr::mutate("es50_age_{age}"    := NA_real_,
                      "species_age_{age}" := NA_real_,
                      "phylum_age_{age}"  := NA_real_,
                      "records_age_{age}" := NA_real_,
                      .before = geom_ix)
    }
  } else {
    if (verbose) cat(paste(mpa$WDPAID, "No records, age:", age, "\n", sep=" "))

    geom_ix <- which_geometry(mpa)
    
    mpa <- mpa %>%
      dplyr::mutate("es50_age_{age}"    := NA_real_,
                    "species_age_{age}" := NA_real_,
                    "phylum_age_{age}"  := NA_real_,
                    "records_age_{age}" := NA_real_,
                    .before = geom_ix)
  }

  return(mpa)

}
