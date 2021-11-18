#' Generate a sf object of random points within and around a set of polygons.
#'
#' Polygons are expanded by \code{buffer}, and then samples are drawn from
#' those.  See \code{\link[sf]{st_buffer}}
#'
#' @export
#' @param x sf object of polygons
#' @param buffer numeric See \code{\link[sf]{st_buffer}}
#' @param n integer, the number of points
#' @return a sf object of n points
random_points <- function(x = read_mpa("Cuba"),
                          buffer = 0.1,
                          n = 1000){

  pts <- suppressMessages(suppressWarnings(sf::st_buffer(x, dist = buffer) %>%
                            sf::st_sample(size = n)))
  return(pts)
}


#' Given a set of MPAs and observations determine which observations belong to which MPA
#'
#' @export
#' @param mpa sf object of MPA polygons
#' @param obs sf object of observation points
#' @param ... other arguments for \code{\link[sf]{st_intersects}}
#' @return a list of match vectors by index. If obs has 1000 points
#'   then a 1000 length list of integer vectors is returned where each
#'   vector has zero or more integers indicating which mpa polygon each
#'   belongs to. For example, the first 3 points below do not intersect
#'   any polygons, while the 4th intersects with 3 polygons and the
#'   the 5th intersects with just one.
#'  \itemize{
#'    \item{ 1 int(0)}
#'    \item{ 2 int(0)}
#'    \item{ 3 int(0)}
#'    \item{ 4 int [1:3] 42 71 177}
#'    \item{ 5 int 40}
#'  }
mpa_match <- function(mpa = read_mpa("Cuba"),
                      obs = random_points(mpa),
                      ...){

  z <- sf::st_intersects(obs, mpa, ...)
  return(z)
}



#' Fetch MPA data by country from WDPA.  Be patient with this - it can take a while.
#'
#'
#' @export
#' @param name character, either accepted country name or ISO-3 code. You can also
#'   downlaod the global dataset using \code{global}.
#' @param path character, the output path.  We default to that specified by
#'  \code{\link[rappdirs]{user_data_dir}}
#' @param ext character, the file extension, by default we use ".gpkg"
#' @param clean logical, if TRUE run the result through \code{\link[wdpar]{wdpa_clean}}
#' @param overwrite logical, if TRUE overwrite exisiting datasets
#' @param ... other arguments for , see \code{\link[wdpar]{wdpa_fetch}}
#' @return sf table of protected areas
#' @examples
#' \dontrun{
#' x <- fetch_mpa(name = 'Cuba')
#' plot((x %>% dplyr::filter(MARINE == "1"))['REP_AREA'])
#' }
fetch_mpa <- function(name = 'Cuba',
                     path = rappdirs::user_data_dir("wdpar"),
                     ext = ".gpkg",
                     clean = TRUE,
                     overwrite = TRUE,
                     ...){
  if(!dir.exists(path)){
    ok <- dir.create(path, showWarnings = FALSE, recursive = TRUE)
    if(!dir.exists(path)) stop("wpdar output path doesn't exist:", path)
  }
  x <- wdpar::wdpa_fetch(name[1],
                         wait = TRUE,
                         download_dir = tempdir(),
                         ...)

  if (clean)  x <- wdpar::wdpa_clean(x) %>%
    sf::st_transform("EPSG:4326")

  y <- sf::write_sf(x, file.path(path, paste0(name[1], ext[1])),
               delete_dsn = overwrite)
  return(x)
}

#' Read a previously downloaded MPA dataset for a country, region, etc
#'
#' @export
#' @param name character, the name of the country or 'global' that you have previously
#'  downloaded
#' @param path character, the output path.  We default to \code{\link[rappdirs]{user_data_dir}}
#' @param ext character, the file extension, by default we use ".gpkg"
#' @return a simple features table
#' @examples
#' \dontrun{
#'   require(sf)
#'   x <- mpa_read("Cuba")
#'   plot(x['REP_AREA'])
#' }
read_mpa <- function(name = 'Cuba',
                     path = rappdirs::user_data_dir("wdpar"),
                     ext = ".gpkg"){
  filename <- file.path(path, paste0(name[1], ext[1]))
  if (!file.exists(filename[1])) stop("file not found: ", filename[1])
  suppressMessages(sf::read_sf(filename[1]))

}


#' List the locally stored MPAs
#'
#' @export
#' @param path the path to storage
#' @param ext character, the file extension, by default we use ".gpkg"
#' @param full_names logical, if TRUE return full filenames
list_mpa <- function(path = rappdirs::user_data_dir("wdpar"),
                     ext = ".gpkg",
                     full_names = FALSE){
  ff <- list.files(path, pattern = glob2rx(paste0("*", ext[1])), full.names = TRUE)
  if (!full_names){
    ff <- gsub(ext, "", basename(ff), fixed = TRUE)
  }
  ff
}

#' Retrieve the projected CRS for WDPA cleaned (ESRI:54017) data
#'
#' @export
#' @param form character one of 'proj4string', 'epsg', or 'wkt'
#' @return character
wdpa_cleaned_crs <- function(form = c('proj4string', 'epsg', 'wkt')[3]){

  switch(tolower(form[1]),
         'epsg' = 'ESRI:54017',
         'proj4string' = "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
         "PROJCS[\"unknown\",\n    GEOGCS[\"unknown\",\n        DATUM[\"WGS_1984\",\n            SPHEROID[\"WGS 84\",6378137,298.257223563],\n            AUTHORITY[\"EPSG\",\"6326\"]],\n        PRIMEM[\"Greenwich\",0,\n            AUTHORITY[\"EPSG\",\"8901\"]],\n        UNIT[\"degree\",0.0174532925199433]],\n    PROJECTION[\"Cylindrical_Equal_Area\"],\n    PARAMETER[\"standard_parallel_1\",30],\n    PARAMETER[\"central_meridian\",0],\n    PARAMETER[\"false_easting\",0],\n    PARAMETER[\"false_northing\",0],\n    UNIT[\"metre\",1,\n        AUTHORITY[\"EPSG\",\"9001\"]],\n    AXIS[\"Easting\",EAST],\n    AXIS[\"Northing\",NORTH]]")
}
