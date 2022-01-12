

library(mpatools)
library(dplyr)

ages <- c(-20, 0, 20)
time_step <- 20

# either read the locally stored dataset, or fetch if not stored locally
country <- "USA"
listing <- list_mpa()
if (!(country %in% lisiting)) {
  mpa <- fetch_mpa(country)
} else {
  mpa <- read_mpa(country)
}



# for testing on a small number of mpas, skip for running the analysis on a whole country
# we can't know ahead of time if we are going to get a column named 'geom' or 'geometry'
# select only MPAs that are polygons (no points or other non-areal objects)
# group by MPA
# compute es_50 by group
# bind results into one dataframe (one row per MPA)
# drop the geometry to leave a bare table
xx <- mpa %>% 
  head(5) %>%
  dplyr::select(WDPAID, 
                NAME, 
                IUCN_CAT,
                REP_AREA,
                REP_M_AREA, 
                STATUS, 
                STATUS_YR, 
                dplyr::starts_with("geom")) %>% 
  dplyr::filter(sapply(dplyr::starts_with("geom"), function(x) inherits(x, "MULTIPOLYGON"))) %>% 
  dplyr::group_by(WDPAID) %>% 
  dplyr::group_map(es50_base, .keep=TRUE) %>% 
  #dplyr::group_map(es50_timeblock, ages, .keep=TRUE) %>% 
  dplyr::bind_rows() %>% 
  sf::st_drop_geometry()

