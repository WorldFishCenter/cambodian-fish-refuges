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


test_sampling_info <- function(sampling_info){
  multiple_rows_per_sampling <- sampling_info %>%
    has_single_row(sampling)

  if (multiple_rows_per_sampling)
    stop("Multiple rows per sampling in sammpling_info table")
}


get_site_info <- function(fish_data_raw){
  fish_data_raw %>%
    dplyr::select(cfr_name, cfr_id, village, commune, district, province,
                  agro_eco_zone, fi_a_designated, category, category_1) %>%
    dplyr::distinct() %>%
    dplyr::mutate(refuge = make_id(cfr_id, "fr"))
}

test_site_info <- function(site_info){
  site_info %>%
    has_single_row(sampling)

  if (multiple_rows_per_sampling)
    stop("Multiple rows per sampling in sammpling_info table")
}

# make an id based on a prefix and a number
make_id <- function(x, prefix){
  stopifnot(is.numeric(x))
  paste0(prefix,
         stringr::str_pad(string = x,
                          width = floor(log10(max(x))) + 1,
                          pad = "0"))
}

# check whether there is a single row for each value in column_name
has_single_row <- function(df, column_name){
  df %>%
    dplyr::count({{column_name}}) %$%
    magrittr::is_greater_than(n, 1) %>%
    any()
}
