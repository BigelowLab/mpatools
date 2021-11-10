#' Ensure that a set of OBIS observations actually fall within a set of MPAs
#'
#' @param obs tibble of OBIS observations
#' @param mpa sf MULTIPOLYGON object of MPAs form WDPA
#' @param form character one of
#' \itemize{
#' \item{tibble return a tibble}
#' \item{sf return an sf object (spatially referenced tibble)}
#' \item{index a list of indices that provide the match between obs and mpas}
#' }
#' @return tibble of obs with possibly fewer rows
#' @examples
#' \dontrun{
#'   mpa = read_mpa("Belgium")
#'   obs = read_obis("Belgium")
#'   obs_strict <- obis_strict_match(obs, mpa)
#'   dim(obs)
#'   # [1] 511490    161
#'   dim(obs_strict)
#'   # [1] 480302    161
#' }
obis_strict_match <- function(obs = read_obis("Cuba"),
                              mpa = read_mpa("Cuba"),
                              form = c("tibble", "sf", "index")[1]){

  obsf <- obis_as_sf(obs, crs = sf::st_crs(mpa))
  ix <- mpa_match(mpa, obsf)

  switch(tolower(form[1]),
         "tibble" = obs %>% dplyr::filter(lengths(ix) > 0),
         "sf"     =  obsf %>% dplyr::filter(lengths(ix) > 0),
         ix) # anything else
}



#' Cast an OBIS data frame as a simple feature (POINT)
#'
#' @param x tibble of OBIS data
#' @param transform_crs WKT, by default wdpa_cleaned_crs("wkt")
#' @param ... other arguments for \code{\link[sf]{st_as_sf}}
#' @return sf object (POINT)
obis_as_sf <- function(x,
                       crs = c(NA, wdpa_cleaned_crs("wkt"))[2],
                       ...){
  x <- sf::st_as_sf(x, coords = c("decimalLongitude", "decimalLatitude"), ...)
  if (!is.na(crs)) x <- sf::st_transform(x, crs)
  x
}


#' Given a simple features tibble (POLYGON or MUTLIPOLYGON), retrieve OBIS observation records.
#'
#' @param x MPA sf object such as for a country
#' @param policy character either "convex hull" or "strict"
#'    We fetch the convex hull to simplify the fetch, but that may allow some observations that are outside MPAs.
#'    If "convex hull" then we allow these to be returned.  If "strict" then we filter for those observations that are in
#'    the MPAs (in mean within or on the boundary).
#' @return tibble of OBIS observations, possibly empty
fetch_obis <- function(x = read_wdpa_country("Cuba"),
                           policy = c("convex hull", "strict")[1]) {


  # Fetch one row (one MPA)
  # @param x a single-row sf POLYGON or MULTIPOLYGON object
  # @param key, used by dplyr to store the value of the gouping variable for this iteration but ignored here
  # @return NULL (upon error) or tibble of OBIS results
  get_one_mpa <- function(x, key){
    r <- try(robis::occurrence(geometry = sf::st_as_text(sf::st_convex_hull(sf::st_geometry(x)))))
    if (inherits(r, "try-error")){
      warning("error fetching obis datafir WDPAID:", x$WDPAID[1])
      r <- NULL
    }
    r
  }

  r <- x %>%
    dplyr::rowwise() %>%
    dplyr::group_map(get_one_mpa,
                     .keep = TRUE) %>%
    dplyr::bind_rows()

  if (tolower(policy[1]) == "strict"){
    r <- obis_strict_match(x, r)
  }

  r
}

#' Fetch OBIS data by country
#'
#' No error checking is done to ensure the country folderand WDPA file
#'
#' @param mpa POLYGON or MULTIPOLYGON as sf
#' @param name character, the name to save the MPA under (no spaces please!)
#' @param save_file logical, if TRUE save into the country's data folder
#' @param ... other arguments for obis_fetch_mpa
#' @return tibble of OBIS observations, possibly empty
fetch_obis_country <- function(name = "Cuba",
                           overwrite = TRUE,
                           ...){
                             
  #if(!dir.exists(path)){
  #  ok <- dir.create(path, showWarnings = FALSE, recursive = TRUE)
  #  if(!dir.exists(path)) stop("wpdar output path doesn't exist:", path)
  #}
  
  mpa <- try(read_mpa(name))
  if (inherits(mpa, 'try-error')) stop("you must provide an mpa")


  fetch_obis(mpa,...) %>%
    write_obis(name, overwrite = overwrite)
}


#' Read an OBIS country file
#'
#' @param name character, the name of the dataset
#' @param path character, the output path.  We default to that specified by
#'  \code{\link[rappdirs]{user_data_dir}}
#' @param form chracter, either 'table' (default) or 'sf'
#' @param ... further arguments for \code{\link{obis_as_sf}} in piarricular \code{crs}
#' @return data table or sf object
read_obis <- function(name = "Cuba",
                      path = rappdirs::user_data_dir("robis"),
                      form  = c("table", "sf")[1],
                      ...){
  filename <- file.path(path, sprintf("%s.csv.gz", name))
  if (!file.exists(filename)){
    stop("file not found:", filename)
  }
  x <- readr::read_csv(filename)
  if (tolower(form[1]) == "sf"){
    x <- obis_as_sf(x, ...)
  }
}

#' Write an OBIS table to csv
#'
#' @param x OBIS table of data
#' @param name character the name to save the dataset under (no spaces please!)
#' @param overwrite logical, if TRUE allow existing files to be overwritten
#' @return the input data table
write_obis <- function(x, name,
                       path = rappdirs::user_data_dir("robis"),
                       overwrite = FALSE){

  ofile <- file.path(path, sprintf("%s.csv.gz", name))
  if (file.exists(filename) && overwrite == FALSE){
    stop("file already exists:", filename)
  }
  readr::write_csv(x, filename)
}
