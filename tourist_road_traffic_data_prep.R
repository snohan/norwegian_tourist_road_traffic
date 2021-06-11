# Fetch data, wrangle and save
# To avoid the Rmd file to do this for every rendering

# API call functions
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
#source("H:/Programmering/R/byindeks/index_report_functions.R")

# Trps ----
points <- get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(name = stringr::str_to_title(name, locale = "no"))

points_metadata <- points %>%
  split_road_system_reference() %>%
  dplyr::select(trp_id, name, road_category_and_number,
                county_geono, county_name, municipality_name,
                lat, lon, road_network_position, road_network_link, road_link_position)


# Tourist road info ----
tourist_road_info <- get_national_tourist_roads()

# NB! Never view the geometry - slows everything!
tourist_roads <- tourist_road_info$geometri %>%
  sf::st_drop_geometry() %>%
  dplyr::select(objekt_id, Navn) %>%
  saveRDS(file = "data/tourist_road_names.rds")

trps_on_tourist_roads <- points_metadata %>%
  dplyr::filter(road_network_link %in% tourist_road_info$veglenkeposisjoner$veglenkesekvensid) %>%
  dplyr::left_join(tourist_road_info$veglenkeposisjoner,
                   by = c("road_network_link" = "veglenkesekvensid")) %>%
  dplyr::distinct(trp_id, .keep_all = TRUE)

n_points_per_road <- trps_on_tourist_roads %>%
  dplyr::group_by(Navn) %>%
  dplyr::summarise(n_trps = n())
