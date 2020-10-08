# get clean information about the sampling ocassions
get_occasion_info <- function(community_data){
  community_data %>%
    dplyr::select(refuge, occasion, sampling, date, total_hours, gauge_start,
                  gauge_finish, rf_water_level, water_temp, secchi_depth,
                  phosphate, nitrogen, conductivity, aquatic_plant_area,
                  aquatic_plant_density, brush_park, illegal_fishing,
                  water_bird, other_animal, weather)  %>%
    dplyr::distinct()

}

# Check the sampling information looks alright
test_occasion_info <- function(occasion_info){
  multiple_rows_per_sampling <- occasion_info %>%
    has_single_row(sampling)

  if (multiple_rows_per_sampling)
    stop("Multiple rows per sampling in sammpling_info table")
}
