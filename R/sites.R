# Get a data frame with site information for further use
get_site_info <- function(fish_data_raw){
  fish_data_raw %>%
    dplyr::select(cfr_name, cfr_id, village, commune, district, province,
                  agro_eco_zone, fi_a_designated, category_1) %>%
    dplyr::rename(category = category_1) %>%
    dplyr::distinct() %>%
    dplyr::mutate(refuge = make_id(cfr_id, "fr"))
}

# Check the site information looks alright
test_site_info <- function(site_info){
  multiple_rows_per_site <- site_info %>%
    has_single_row(refuge)

  if (multiple_rows_per_site)
    stop("Multiple rows per sampling in sammpling_info table")
}
