

devtools::load_all()

# Choose a country here
country <- "South Africa"

#If you have already stored the country's wdpa data on your machine, use read_country
#Otherwise, use fetch country to download the wdpas. This step could take a few minutes
mpa <- read_mpa(country)
mpa <- fetch_mpa(country)


#for testing on a small number of mpas, skip for running the analysis on a whole country
mpa <- head(mpa, 5)

#ages <- c(-20, 0, 20)
#time_step <- 20

xx <- mpa %>% 
  dplyr::select(WDPAID, 
                NAME, 
                IUCN_CAT,
                REP_AREA,
                REP_M_AREA, 
                STATUS, 
                STATUS_YR, 
                geom) %>% 
  dplyr::filter(sapply(.data$geom, function(x) inherits(x, "MULTIPOLYGON"))) %>% 
  dplyr::group_by(WDPAID) %>% 
  dplyr::group_map(es50_base, .keep=TRUE) %>% 
  #dplyr::group_map(es50_timeblock, ages, .keep=TRUE) %>% 
  dplyr::bind_rows() %>% 
  sf::st_drop_geometry()

