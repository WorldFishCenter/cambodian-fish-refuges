# get clean information about the sampling ocassions
get_sampling_info <- function(fish_data_raw){
  fish_data_raw %>%
    dplyr::select(no, date, total_hours, gauge_start, gauge_finish,
                  rf_water_level, water_tem,secchi_depth, phosphate, nitrogen,
                  conductivity, aquatic_plant_area, aquatic_plant_density,
                  brush_park, illegal_fishing_seen, water_bird, other_animal,
                  weather)  %>%
    dplyr::rename(water_temp = water_tem,
                  illegal_fishing = illegal_fishing_seen) %>%
    dplyr::distinct() %>%
    dplyr::mutate(sampling = make_id(no, "sa")) %>%
    dplyr::mutate(date = as.Date(date, origin = "1899-12-30")) %>%
    dplyr::mutate_at(dplyr::vars("illegal_fishing",
                                 "water_bird",
                                 "other_animal",
                                 "weather"), tolower) %>%
    dplyr::mutate_at(dplyr::vars("illegal_fishing",
                                 "water_bird",
                                 "other_animal"), ~ !. == "no")
}

# Check the sampling information looks alright
test_sampling_info <- function(sampling_info){
  multiple_rows_per_sampling <- sampling_info %>%
    has_single_row(sampling)

  if (multiple_rows_per_sampling)
    stop("Multiple rows per sampling in sammpling_info table")
}
