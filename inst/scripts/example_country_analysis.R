

devtools::load_all()


country <- "Belgium"

ages <- c(-20, 0, 20)
time_step <- 20

#fetch_mpa(country)
mpa <- read_mpa(country)


#for testing on a small number of mpas
mpa <- head(mpa, 3)

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
  dplyr::bind_rows()

future::doparallel()

