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

trp_data_time_span <- get_trp_data_time_span()

points_metadata <- points %>%
  split_road_system_reference() %>%
  dplyr::left_join(trp_data_time_span, by = "trp_id") %>%
  dplyr::select(trp_id, name, road_category_and_number,
                county_geono, county_name, municipality_name,
                lat, lon, road_network_position, road_network_link, road_link_position,
                first_data_with_quality_metrics)


# Tourist road info ----
tourist_road_info <- get_national_tourist_roads()

## Geometry ----
# NB! Never view the geometry - slows everything!
tourist_road_geometry <- tourist_road_info$geometri

tourist_road_geometry %>%
  saveRDS(file = "data/tourist_road_geometry.rds")

## Names ----
tourist_road_names <- tourist_road_info$geometri %>%
  sf::st_drop_geometry() %>%
  dplyr::select(objekt_id, Navn) %>%
  dplyr::arrange(Navn)

tourist_road_names %>%
  saveRDS(file = "data/tourist_road_names.rds")

## TRPs ----
trps_on_tourist_roads <- points_metadata %>%
  dplyr::filter(road_network_link %in% tourist_road_info$veglenkeposisjoner$veglenkesekvensid) %>%
  dplyr::left_join(tourist_road_info$veglenkeposisjoner,
                   by = c("road_network_link" = "veglenkesekvensid")) %>%
  dplyr::distinct(trp_id, .keep_all = TRUE) %>%
  dplyr::mutate(name = stringr::str_replace_all(name, "E10 ", ""),
                name = stringr::str_replace_all(name, " \\(Fv27\\)", ""),
                name = stringr::str_replace_all(name, " \\(Fv51\\)", ""),
                name = stringr::str_replace_all(name, " \\(Fv82\\)", ""),
                road_category_and_number_and_point_name =
                  paste0(road_category_and_number, " ", name))

n_points_per_road <- trps_on_tourist_roads %>%
  dplyr::group_by(objekt_id) %>%
  dplyr::summarise(n_trps = n())

# road_names, but do we have a trp on all?
road_names_per_road <- trps_on_tourist_roads %>%
  dplyr::select(objekt_id, road_category_and_number) %>%
  dplyr::distinct() %>%
  dplyr::group_by(objekt_id) %>%
  dplyr::summarise(roads = paste(road_category_and_number, collapse = ", "))

counties_per_road <- trps_on_tourist_roads %>%
  dplyr::select(objekt_id, county_name) %>%
  dplyr::distinct() %>%
  dplyr::group_by(objekt_id) %>%
  dplyr::summarise(counties = paste(county_name, collapse = ", "))

trps_on_tourist_roads <- trps_on_tourist_roads %>%
  dplyr::left_join(n_points_per_road, by = "objekt_id") %>%
  dplyr::left_join(road_names_per_road, by = "objekt_id") %>%
  dplyr::left_join(counties_per_road, by = "objekt_id")

trps_on_tourist_roads %>%
  saveRDS(file = "data/trps_on_tourist_roads.rds")


# MDTs ----
mdt_2018 <- get_mdt_for_trp_list(trps_on_tourist_roads$trp_id, "2018")
mdt_2019 <- get_mdt_for_trp_list(trps_on_tourist_roads$trp_id, "2019")
mdt_2020 <- get_mdt_for_trp_list(trps_on_tourist_roads$trp_id, "2020")
mdt_2021 <- get_mdt_for_trp_list(trps_on_tourist_roads$trp_id, "2021")

mdts <- dplyr::bind_rows(
  mdt_2018,
  mdt_2019,
  mdt_2020,
  mdt_2021
) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::select(trp_id, year, month, mdt) %>%
  tidyr::complete(trp_id = trps_on_tourist_roads$trp_id, year, month)

# trp_mdt_wide <- trps_on_tourist_roads %>%
#   dplyr::left_join(mdts, by = "trp_id") %>%
#   dplyr::mutate(month_object = lubridate::make_date(year = year, month = month),
#                 month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)) %>%
#   dplyr::select(Navn, trp_id, name, road_category_and_number, year, month_name, mdt) %>%
#   tidyr::pivot_wider(names_from = month_name, values_from = mdt) %>%
#   dplyr::select(Navn, name, road_category_and_number, year:august)

# TODO: sort på roadref

# trp_mdt_wide %>%
#   saveRDS(file = "data/trp_mdt_wide.rds")

trp_mdt_long <- trps_on_tourist_roads %>%
  dplyr::left_join(mdts, by = "trp_id") %>%
  dplyr::mutate(month_object = lubridate::make_date(year = 2000, month = month),
                month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)) %>%
  dplyr::select(Navn, trp_id, road_category_and_number_and_point_name, year, month_object, mdt)

# TODO: sort på roadref

trp_mdt_long %>%
  saveRDS(file = "data/trp_mdt_long.rds")

# Point index ----
pi_2019 <- get_pointindices_for_trp_list(trps_on_tourist_roads$trp_id, "2019")
pi_2020 <- get_pointindices_for_trp_list(trps_on_tourist_roads$trp_id, "2020")
pi_2021 <- get_pointindices_for_trp_list(trps_on_tourist_roads$trp_id, "2021")

trp_id_objekt_id <- trps_on_tourist_roads %>%
  dplyr::select(trp_id, objekt_id)

pis <- dplyr::bind_rows(
  pi_2019,
  pi_2020,
  pi_2021
  ) %>%
  dplyr::filter(month %in% c(6, 7, 8),
                day_type == "ALL",
                period == "month") %>%
  dplyr::select(trp_id, year, month, index_total, index_total_p,
                base_volume, calculation_volume) %>%
  dplyr::left_join(trp_id_objekt_id, by = "trp_id")


# Tourist road index ----
## Per road ----
index_per_road_month <- pis %>%
  dplyr::group_by(objekt_id, year, month) %>%
  dplyr::summarise(n_points = n(),
                   base_volume = sum(base_volume, na.rm = TRUE),
                   calculation_volume = sum(calculation_volume, na.rm = TRUE),
                   index_i = calculation_volume / base_volume,
                   index_p = (index_i - 1) * 100) %>%
  dplyr::mutate(index_period = paste0(year - 1, " - ", year),
                month_object = lubridate::make_date(year = year, month = month),
                month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(objekt_id, index_period, month_name, index_p) %>%
  tidyr::pivot_wider(names_from = month_name, values_from = index_p)

# TODO:
# trp_index_from_refyear <- this_citys_trp_index %>%
#   dplyr::select(trp_id, tidyselect::starts_with("index")) %>%
#   dplyr::filter(
#     dplyr::across(
#       .cols = tidyselect::starts_with("index"),
#       .fns = ~ !is.na(.x)
#     )
#   ) %>%
#   dplyr::mutate(
#     dplyr::across(
#       .cols = tidyselect::starts_with("index"),
#       .fns = ~ index_converter(.))) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) %>%
#   dplyr::mutate(index = round(100 * (index - 1), digits = 1)) %>%
#   dplyr::select(trp_id, index)

index_per_road <- pis %>%
  dplyr::group_by(objekt_id, year) %>%
  dplyr::summarise(n_points = n(),
                   base_volume = sum(base_volume, na.rm = TRUE),
                   calculation_volume = sum(calculation_volume, na.rm = TRUE),
                   index_i = calculation_volume / base_volume,
                   index_p = (index_i - 1) * 100) %>%
  dplyr::mutate(index_period = paste0(year - 1, " - ", year)) %>%
  dplyr::ungroup() %>%
  dplyr::select(objekt_id, index_period, summer = index_p)

index_per_road_and_per_month <- index_per_road_month %>%
  dplyr::left_join(index_per_road, by = c("objekt_id", "index_period")) %>%
  dplyr::left_join(tourist_road_names, by = "objekt_id") %>%
  dplyr::select(-objekt_id) %>%
  dplyr::relocate(Navn, .before = index_period)

index_per_road_and_per_month %>%
  saveRDS(file = "data/index_per_road_and_per_month.rds")

## All roads in one ----
index_per_month <- pis %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(n_points = n(),
                   base_volume = sum(base_volume, na.rm = TRUE),
                   calculation_volume = sum(calculation_volume, na.rm = TRUE),
                   index_i = calculation_volume / base_volume,
                   index_p = (index_i - 1) * 100) %>%
  dplyr::mutate(index_period = paste0(year - 1, " - ", year),
                month_object = lubridate::make_date(year = year, month = month),
                month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(index_period, month_name, index_p) %>%
  tidyr::pivot_wider(names_from = month_name, values_from = index_p)

index <- pis %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n_points = n(),
                   base_volume = sum(base_volume, na.rm = TRUE),
                   calculation_volume = sum(calculation_volume, na.rm = TRUE),
                   index_i = calculation_volume / base_volume,
                   index_p = (index_i - 1) * 100) %>%
  dplyr::mutate(index_period = paste0(year - 1, " - ", year)) %>%
  dplyr::ungroup() %>%
  dplyr::select(index_period, summer = index_p)

index_summer_and_month <- index_per_month %>%
  dplyr::left_join(index, by = "index_period")

index_summer_and_month %>%
  saveRDS(file = "data/index_summer_and_month.rds")
