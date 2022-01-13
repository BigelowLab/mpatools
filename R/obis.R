#' Ensure that a set of OBIS observations actually fall within a set of MPAs
#'
#' @export
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
#' @export
#' @param x tibble of OBIS data
#' @param crs WKT or numeric, 4326 or NA to skip
#' @param ... other arguments for \code{\link[sf]{st_as_sf}}
#' @return sf object (POINT)
obis_as_sf <- function(x,
                       crs = c(NA, 4326)[2],
                       ...){
  x <- sf::st_as_sf(x, coords = c("decimalLongitude", "decimalLatitude"), crs = crs, ...)
  if (!is.na(crs)) x <- sf::st_transform(x, crs)
  x
}


#' Given a simple features tibble (POLYGON or MUTLIPOLYGON), retrieve OBIS observation records.
#'
#' @export
#' @param x MPA sf object such as for a country
#' @param combine logical, if TRUE then combine all MPAs into one convex polygon. This reduces the
#'   network from many small ones to a single large one.  Use this to reduce the number of calls
#'   to OBIS, the trade off is that the request may many records that are subsequently filtered
#'   out if \code{policy} is 'strict'. 
#' @param policy character either "convex hull" or "strict"
#'    We fetch the convex hull to simplify the fetch, but that may allow some observations that are outside MPAs.
#'    If "convex hull" then we allow these to be returned.  If "strict" then we filter for those observations that are in
#'    the MPAs (in mean within or on the boundary).
#' @param form character, either 'table' (default) or 'sf'
#' @return tibble of OBIS observations, possibly empty
fetch_obis <- function(x = read_mpa("Cuba"),
                       combine = TRUE, 
                       policy = c("convex hull", "strict")[1],
                       form = c("tibble", "sf")[1]) {


  # Fetch one row (one MPA)
  # @param x a single-row sf POLYGON or MULTIPOLYGON object
  # @param key, used by dplyr to store the value of the gouping variable for this iteration but ignored here
  # @return NULL (upon error) or tibble of OBIS results
  get_one_mpa <- function(x, key){
    geo <- sf::st_geometry(x) %>%
      sf::st_simplify(dTolerance = 1000) %>%
      sf::st_convex_hull() %>%
      sf::st_as_text()
    r <- try(robis::occurrence(geometry = geo))
    if (inherits(r, "try-error")){
      warning("error fetching obis data for WDPAID:", x$WDPAID[1])
      r <- NULL
    }
    r
  }

  if (combine){
    geo <- x %>%
      sf::st_geometry() %>%
      sf::st_combine() %>%
      sf::st_convex_hull() %>%
      sf::st_as_text()
    
    r <- try(robis::occurrence(geometry = geo))
    if (inherits(r, "try-error")){
      warning("error fetching obis data")
      r <- dplyr::tibble()
    }
    
  } else {
    r <- x %>%
      dplyr::rowwise() %>%
      dplyr::group_map(get_one_mpa,
                       .keep = TRUE) %>%
      dplyr::bind_rows()
  }
  
  if (tolower(policy[1]) == "strict"){
    r <- obis_strict_match(x, r)
  }

  if (tolower(form[1]) == "sf") r <- obis_as_sf(r)
  r
}

#' Fetch OBIS data by country
#'
#' No error checking is done to ensure the country folderand WDPA file
#'
#' @export
#' @param name character, the name of the country to fetch
#' @param overwrite logical, if TRUE allow existing files to be overwritten
#' @param ... other arguments for obis_fetch_mpa
#' @return tibble of OBIS observations, possibly empty
fetch_obis_country <- function(name = "Cuba",
                           overwrite = TRUE,
                           ...){

  mpa <- try(read_mpa(name))
  if (inherits(mpa, 'try-error')) stop("you must provide an mpa")


  fetch_obis(mpa,...) %>%
    write_obis(name, overwrite = overwrite)
}


#' Read an OBIS country file
#'
#' @export
#' @param name character, the name of the dataset
#' @param path character, the input path.  We default to that specified by \code{\link[rappdirs]{user_data_dir}}
#' @param what character, by default all values are returned, but specify a subset with
#'   a character vector of desired columns (variables)
#' @param form character, either 'table' (default) or 'sf'
#' @param ... further arguments for \code{\link{obis_as_sf}} in particular \code{crs}
#' @return data table or sf object
read_obis <- function(name = "Cuba",
                      path = rappdirs::user_data_dir("robis"),
                      form  = c("table", "sf")[1],
                      what = "all",
                      ...){
  filename <- file.path(path, sprintf("%s.tsv.gz", name))
  if (!file.exists(filename)){
    stop("file not found:", filename)
  }

  x <- data.table::fread(filename, quote="", fill = TRUE) %>%
    dplyr::as_tibble()

  if (tolower(form[1]) == "sf"){
    x <- obis_as_sf(x, ...)
  }
  x
}

#' Write an OBIS table to csv
#'
#' @export
#' @param x OBIS table of data
#' @param name character the name to save the dataset under (no spaces please!)
#' @param path character, the output path.  We default to that specified by
#'  \code{\link[rappdirs]{user_data_dir}}
#' @param overwrite logical, if TRUE allow existing files to be overwritten
#' @return the input data table
write_obis <- function(x, name,
                       path = rappdirs::user_data_dir("robis"),
                       overwrite = FALSE){

  if(!dir.exists(path)){
    ok <- dir.create(path, showWarnings = FALSE, recursive = TRUE)
    if(!dir.exists(path)) stop("robis output path doesn't exist:", path)
  }

  filename <- file.path(path, sprintf("%s.tsv.gz", name))
  if (file.exists(filename) && overwrite == FALSE){
    stop("file already exists:", filename)
  }
  readr::write_tsv(x, filename)
}


#' List the locally stored OBIS datasets
#'
#' @export
#' @param path the path to storage
#' @param ext character, the file extension, by default we use ".csv.gz"
#' @param full_names logical, if TRUE return full filenames
list_obis <- function(path = rappdirs::user_data_dir("robis"),
                     ext = ".csv.gz",
                     full_names = FALSE){

  ff <- list.files(path, pattern = glob2rx(paste0("*", ext[1])), full.names = TRUE)
  if (!full_names){
    ff <- gsub(ext, "", basename(ff), fixed = TRUE)
  }
  ff
}


#' Determine if the local storage contains a set of OBOS records by name(s)
#' 
#' @export
#' @param name character, the name(s) of the country or 'global' that you have previously
#'  downloaded
#' @param ... arguments for \code{list_mpa}
#' @return one or more logicals as a named vector
has_obis <- function(name = c('Cuba', "Mongolia"),
                    ...){
  nn <- list_obis(...)
  ix <- name %in% nn
  names(ix) <- name
  ix
}


#' Retrieve a list of known column names and \code{\link[readr]{cols}} declarations
#'
#' @export
#' @param what character, by default all values are returned, but specify a subset with
#'  a character vector of desired columns (variables).  All other columns will be set to
#'  \code{\link[readr]{col_skip}}
#' @return names list
obis_columns <- function(what = "all"){
  COL_TYPES <- list(
    id = readr::col_character(),
    dataset_id = readr::col_character(),
    decimallongitude = readr::col_double(),
    decimallatitude = readr::col_double(),
    date_start = readr::col_double(),
    date_mid = readr::col_double(),
    date_end = readr::col_double(),
    date_year = readr::col_double(),
    scientificname = readr::col_character(),
    originalscientificname = readr::col_character(),
    minimumdepthinmeters = readr::col_double(),
    maximumdepthinmeters = readr::col_double(),
    coordinateuncertaintyinmeters = readr::col_double(),
    flags = readr::col_character(),
    dropped = readr::col_character(),
    absence = readr::col_character(),
    shoredistance = readr::col_double(),
    bathymetry = readr::col_double(),
    sst = readr::col_double(),
    sss = readr::col_double(),
    marine = readr::col_double(),
    brackish = readr::col_double(),
    freshwater = readr::col_double(),
    terrestrial = readr::col_double(),
    taxonrank = readr::col_character(),
    aphiaid = readr::col_double(),
    redlist_category = readr::col_character(),
    superdomain = readr::col_character(),
    domain = readr::col_character(),
    kingdom = readr::col_character(),
    subkingdom = readr::col_character(),
    infrakingdom = readr::col_character(),
    phylum = readr::col_character(),
    phylum_division = readr::col_character(),
    subphylum_subdivision = readr::col_character(),
    subphylum = readr::col_character(),
    infraphylum = readr::col_character(),
    superclass = readr::col_character(),
    class = readr::col_character(),
    subclass = readr::col_character(),
    infraclass = readr::col_character(),
    subterclass = readr::col_character(),
    superorder = readr::col_character(),
    order = readr::col_character(),
    suborder = readr::col_character(),
    infraorder = readr::col_character(),
    parvorder = readr::col_character(),
    superfamily = readr::col_character(),
    family = readr::col_character(),
    subfamily = readr::col_character(),
    supertribe = readr::col_character(),
    tribe = readr::col_character(),
    subtribe = readr::col_character(),
    genus = readr::col_character(),
    subgenus = readr::col_character(),
    section = readr::col_character(),
    subsection = readr::col_character(),
    series = readr::col_character(),
    species = readr::col_character(),
    subspecies = readr::col_character(),
    natio = readr::col_character(),
    variety = readr::col_character(),
    subvariety = readr::col_character(),
    forma = readr::col_character(),
    subforma = readr::col_character(),
    type = readr::col_character(),
    modified = readr::col_character(),
    language = readr::col_character(),
    license = readr::col_character(),
    rightsholder = readr::col_character(),
    accessrights = readr::col_character(),
    bibliographiccitation = readr::col_character(),
    references = readr::col_character(),
    institutionid = readr::col_character(),
    collectionid = readr::col_double(),
    datasetid = readr::col_character(),
    institutioncode = readr::col_character(),
    collectioncode = readr::col_character(),
    datasetname = readr::col_character(),
    ownerinstitutioncode = readr::col_character(),
    basisofrecord = readr::col_character(),
    informationwithheld = readr::col_character(),
    datageneralizations = readr::col_character(),
    dynamicproperties = readr::col_character(),
    materialsampleid = readr::col_character(),
    occurrenceid = readr::col_character(),
    catalognumber = readr::col_character(),
    occurrenceremarks = readr::col_character(),
    recordnumber = readr::col_character(),
    recordedby = readr::col_character(),
    recordedbyid = readr::col_character(),
    individualcount = readr::col_double(),
    organismquantity = readr::col_character(),
    organismquantitytype = readr::col_character(),
    sex = readr::col_character(),
    lifestage = readr::col_character(),
    reproductivecondition = readr::col_character(),
    behavior = readr::col_character(),
    establishmentmeans = readr::col_character(),
    occurrencestatus = readr::col_character(),
    preparations = readr::col_character(),
    disposition = readr::col_character(),
    othercatalognumbers = readr::col_character(),
    associatedmedia = readr::col_character(),
    associatedreferences = readr::col_character(),
    associatedsequences = readr::col_character(),
    associatedtaxa = readr::col_character(),
    organismid = readr::col_character(),
    organismname = readr::col_character(),
    organismscope = readr::col_character(),
    associatedoccurrences = readr::col_character(),
    associatedorganisms = readr::col_character(),
    previousidentifications = readr::col_character(),
    organismremarks = readr::col_character(),
    eventid = readr::col_character(),
    parenteventid = readr::col_character(),
    samplingprotocol = readr::col_character(),
    samplesizevalue = readr::col_character(),
    samplesizeunit = readr::col_character(),
    samplingeffort = readr::col_character(),
    eventdate = readr::col_character(),
    eventtime = readr::col_character(),
    startdayofyear = readr::col_double(),
    enddayofyear = readr::col_double(),
    year = readr::col_double(),
    month = readr::col_double(),
    day = readr::col_double(),
    verbatimeventdate = readr::col_character(),
    habitat = readr::col_character(),
    fieldnumber = readr::col_character(),
    fieldnotes = readr::col_character(),
    eventremarks = readr::col_character(),
    locationid = readr::col_character(),
    highergeographyid = readr::col_character(),
    highergeography = readr::col_character(),
    continent = readr::col_character(),
    waterbody = readr::col_character(),
    islandgroup = readr::col_character(),
    island = readr::col_character(),
    country = readr::col_character(),
    countrycode = readr::col_character(),
    stateprovince = readr::col_character(),
    county = readr::col_character(),
    municipality = readr::col_character(),
    locality = readr::col_character(),
    verbatimlocality = readr::col_character(),
    verbatimelevation = readr::col_character(),
    minimumelevationinmeters = readr::col_character(),
    maximumelevationinmeters = readr::col_character(),
    verbatimdepth = readr::col_character(),
    minimumdistanceabovesurfaceinmeters = readr::col_double(),
    maximumdistanceabovesurfaceinmeters = readr::col_double(),
    locationaccordingto = readr::col_character(),
    locationremarks = readr::col_character(),
    verbatimcoordinates = readr::col_character(),
    verbatimlatitude = readr::col_character(),
    verbatimlongitude = readr::col_character(),
    verbatimcoordinatesystem = readr::col_character(),
    verbatimsrs = readr::col_character(),
    geodeticdatum = readr::col_character(),
    coordinateprecision = readr::col_double(),
    pointradiusspatialfit = readr::col_character(),
    footprintwkt = readr::col_character(),
    footprintsrs = readr::col_character(),
    footprintspatialfit = readr::col_character(),
    georeferencedby = readr::col_character(),
    georeferenceddate = readr::col_character(),
    georeferenceprotocol = readr::col_character(),
    georeferencesources = readr::col_character(),
    georeferenceverificationstatus = readr::col_character(),
    georeferenceremarks = readr::col_character(),
    geologicalcontextid = readr::col_character(),
    earliesteonorlowesteonothem = readr::col_character(),
    latesteonorhighesteonothem = readr::col_character(),
    earliesteraorlowesterathem = readr::col_character(),
    latesteraorhighesterathem = readr::col_character(),
    earliestperiodorlowestsystem = readr::col_character(),
    latestperiodorhighestsystem = readr::col_character(),
    earliestepochorlowestseries = readr::col_character(),
    latestepochorhighestseries = readr::col_character(),
    earliestageorloweststage = readr::col_character(),
    latestageorhigheststage = readr::col_character(),
    lowestbiostratigraphiczone = readr::col_character(),
    highestbiostratigraphiczone = readr::col_character(),
    lithostratigraphicterms = readr::col_character(),
    group = readr::col_character(),
    formation = readr::col_character(),
    member = readr::col_character(),
    bed = readr::col_character(),
    identificationid = readr::col_character(),
    identifiedby = readr::col_character(),
    identifiedbyid = readr::col_character(),
    dateidentified = readr::col_character(),
    identificationreferences = readr::col_character(),
    identificationremarks = readr::col_character(),
    identificationqualifier = readr::col_character(),
    identificationverificationstatus = readr::col_character(),
    typestatus = readr::col_character(),
    taxonid = readr::col_character(),
    scientificnameid = readr::col_character(),
    acceptednameusageid = readr::col_character(),
    parentnameusageid = readr::col_character(),
    originalnameusageid = readr::col_character(),
    nameaccordingtoid = readr::col_character(),
    namepublishedinid = readr::col_character(),
    taxonconceptid = readr::col_character(),
    acceptednameusage = readr::col_character(),
    parentnameusage = readr::col_character(),
    originalnameusage = readr::col_character(),
    nameaccordingto = readr::col_character(),
    namepublishedin = readr::col_character(),
    namepublishedinyear = readr::col_character(),
    higherclassification = readr::col_character(),
    specificepithet = readr::col_character(),
    infraspecificepithet = readr::col_character(),
    verbatimtaxonrank = readr::col_character(),
    scientificnameauthorship = readr::col_character(),
    vernacularname = readr::col_character(),
    nomenclaturalcode = readr::col_character(),
    taxonomicstatus = readr::col_character(),
    nomenclaturalstatus = readr::col_character(),
    taxonremarks = readr::col_character()
  )
  COL_TYPES
}
