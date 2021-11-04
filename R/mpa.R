
#' Fetch WDPA data by country.  Be patient with this - it can take a while.
#'
#' You may get a warning about the "GDAL Error 1: ..." but it doesn't seem to be fatal and
#' the download is unaffected.
#'
#' @export
#' @param country character, either accepted country name or ISO-3 code. You can also
#'   downlaod thge global dataset using \code{global}.
#' @param path character, the output path.  We default to that specified by
#'  \code{\link[rappdirs]{user_data_dir}}
#' @param ext character, the file extension, by default we use ".gpkg"
#' @param clean logical, if TRUE run the resut through \code{\link[wdpar]{wdpa_clean}}
#' @param ... other arguments for , see \code{\link[wdpar]{wdpa_fetch}}
#' @return sf table of protected areas
#' @examples
#' \dontrun{
#' x <- wdpa_fetch_country(country = 'Cuba')
#' plot((x %>% dplyr::filter(MARINE == "1"))['REP_AREA'])
#' }
wdpa_fetch_country <- function(country = 'Cuba',
                               path = rappdirs::user_data_dir("wdpar"),
                               ext = ".gpkg",
                               clean = TRUE,
                               ...){
  if(!dir.exists(path)){
    ok <- dir.create(path, showWarnings = FALSE, recursive = TRUE)
    if(!dir.exists(path)) stop("wpda outout path doesn't exist:", path)
  }
  x <- wdpar::wdpa_fetch(country[1],
                         wait = TRUE,
                         download_dir = tempdir(),
                         ...)
  if (clean) x <- wdpar::wdpa_clean(x)
  y <- sf::write_sf(x, file.path(rappdirs::user_data_dir("wdpar"), paste0(country[1], ext[1])),
               delete_dsn = TRUE)
  return(x)
}

#' Read a previously downloaded WDPA dataset for a country
#'
#' @export
#' @param country character, the name of the country or 'global' if you have previously
#'  downloaded that datatset
#' @param path character, the output path.  We default to \code{\link[rappdirs]{user_data_dir}}
#' @param ext character, the file extension, by default we use ".gpkg"
#' @return a simple features table
#' @examples
#' \dontrun{
#'   require(sf)
#'   x <- wdpa_read_country("Cuba")
#'   plot(x['REP_AREA'])
#' }
wdpa_read_country <- function(country = 'Cuba',
                              path = rappdirs::user_data_dir("wdpar"),
                              ext = ".gpkg"){
  filename <- file.path(path, paste0(country[1], ext[1]))
  if (!file.exists(filename[1])) stop("file not found: ", filename[1])
  sf::read_sf(filename[1])
}


