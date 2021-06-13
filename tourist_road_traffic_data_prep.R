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

## Geometry ----
# NB! Never view the geometry - slows everything!
tourist_road_geometry <- tourist_road_info$geometri

tourist_road_geometry %>%
  saveRDS(file = "data/tourist_road_geometry.rds")

## Names ----
tourist_roads <- tourist_road_info$geometri %>%
  sf::st_drop_geometry() %>%
  dplyr::select(objekt_id, Navn) %>%
  dplyr::arrange(Navn)

tourist_roads %>%
  saveRDS(file = "data/tourist_road_names.rds")

## TRPs ----
trps_on_tourist_roads <- points_metadata %>%
  dplyr::filter(road_network_link %in% tourist_road_info$veglenkeposisjoner$veglenkesekvensid) %>%
  dplyr::left_join(tourist_road_info$veglenkeposisjoner,
                   by = c("road_network_link" = "veglenkesekvensid")) %>%
  dplyr::distinct(trp_id, .keep_all = TRUE)

n_points_per_road <- trps_on_tourist_roads %>%
  dplyr::group_by(objekt_id) %>%
  dplyr::summarise(n_trps = n())

trps_on_tourist_roads <- trps_on_tourist_roads %>%
  dplyr::left_join(n_points_per_road, by = "objekt_id")

trps_on_tourist_roads %>%
  saveRDS(file = "data/trps_on_tourist_roads.rds")


# MDTs ----
mdt_2019 <- get_mdt_for_trp_list(trps_on_tourist_roads$trp_id, "2019")
mdt_2020 <- get_mdt_for_trp_list(trps_on_tourist_roads$trp_id, "2020")
mdt_2021 <- get_mdt_for_trp_list(trps_on_tourist_roads$trp_id, "2021")

mdts <- dplyr::bind_rows(
  mdt_2019,
  mdt_2020,
  mdt_2021
) %>%
  dplyr::filter(coverage > 50,
                month %in% c(6, 7, 8)) %>%
  dplyr::select(trp_id, year, month, mdt) %>%
  tidyr::complete(trp_id = trps_on_tourist_roads$trp_id, year, month)

trp_mdt_wide <- trps_on_tourist_roads %>%
  dplyr::left_join(mdts, by = "trp_id") %>%
  dplyr::mutate(month_object = lubridate::make_date(year = year, month = month),
                month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)) %>%
  dplyr::select(Navn, trp_id, name, road_category_and_number, year, month_name, mdt) %>%
  tidyr::pivot_wider(names_from = month_name, values_from = mdt) %>%
  dplyr::select(Navn, name, road_category_and_number, year:august)

# TODO: sort på roadref

trp_mdt_wide %>%
  saveRDS(file = "data/trp_mdt_wide.rds")

trp_mdt_long <- trps_on_tourist_roads %>%
  dplyr::left_join(mdts, by = "trp_id") %>%
  dplyr::mutate(month_object = lubridate::make_date(year = 2000, month = month),
                month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)) %>%
  dplyr::select(Navn, trp_id, name, road_category_and_number, year, month_object, mdt)

# TODO: sort på roadref

trp_mdt_long %>%
  saveRDS(file = "data/trp_mdt_long.rds")

# Point index ----


# Calculate index per road for each month (6, 7, 8) and total. Since 2019 (or earlier?)
