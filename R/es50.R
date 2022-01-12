#' Record number of records and attempt an overall es50 calculation
#'
#' @export
#' @param mpa a single row of an mpa records table
#' @param key the wdpaid of the mpa
#' @param records an object containing obis records
#' @param verbose logical, if TRUE output helpful messages
#' @return the same mpa row with total_records, total_phyla, total_species, es50_overall
#'  or NULL if an error occurs
#'
es50_base <- function(mpa, key, records, verbose = interactive()) {
  
  if (verbose) cat(key$WDPAID[1], "\n")
  
  # which column is likely to contain geometry?
  geom_ix <- which_geometry(mpa)
  
  # request the records based upon geometry column
  records <- tryCatch(
    {
      robis::occurrence(geometry = sf::st_as_text(sf::st_convex_hull(mpa[[geom_ix[1]]])))
    },
    error = function(e){
      message('Error in es50_base')
      print(e)
      return(NULL)
    })
    
  
  if (!rlang::is_empty(records)) {

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

  }
  
  if (verbose) cat("\n")
  
  return(mpa)
}


#' Calculate es50 metric for an mpa for specific time points
#'
#' @param mpa a row from wdpa table containing one mpa with metadata and geometry column
#' @param key the wdpaid of the mpa
#' @param records an object containing obis records
#' @param age numeric, age to compute es50 for
#' @param step_size numeric of years in each step of analysis
#' @param verbose logical, if TRUE output helpful messages
#' @return single row table containing same columns as mpa but with added timeblock columns
#'
#' @export
es50_timeblock <- function(mpa, key, records, age, step_size, verbose = interactive()) {

  if ((mpa$STATUS_YR + age) > format(as.Date(Sys.Date()), format="%Y")) {
    if (verbose) cat(paste(mpa$WDPAID, "MPA is too young, age:", age, "\n", sep=" "))

    geom_ix <- which_geometry(mpa)
    
    mpa <- mpa %>%
      dplyr::mutate("age_{age}" := NA, .before = geom_ix)

    return(mpa)
  }

  #filter occurrence records for bin calculation
  if (age == 0) {

    age_year <- mpa$STATUS_YR + 19

    age_records <- records %>%
      dplyr::filter(dplyr::between(.data$date_year, mpa$STATUS_YR, age_year))

  } else {
    bin_start <- mpa$STATUS_YR + age

    bin_end <- bin_start + 19

    age_records <- records %>%
      dplyr::filter(dplyr::between(.data$date_year, bin_start, bin_end))
  }

  #compute es50 for filtered occurrence records by bin
  if (nrow(age_records) > 0) {
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
      
      mpa <- mpa %>%
        dplyr::mutate("es50_age_{age}"    := es_50,
                      "species_age_{age}" := nrow(species_counts),
                      "phylum_age_{age}"  := nrow(phylum_counts),
                      "records_age_{age}" := sum(species_counts$x),
                      .before = geom_ix)

      return(mpa)
    } else {
      if (verbose) cat(paste(mpa$WDPAID, "Less than 50 species records, age:", age, "\n", sep=" "))

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
