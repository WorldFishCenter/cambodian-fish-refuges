# Get a data frame with site information for further use
get_refuge_info <- function(community_data){
  community_data %>%
    dplyr::select(refuge, cfr_name, village, commune, district, province,
                  agro_eco_zone, fia_designated,
                  category:rf_area_connected_in_dry_season_ha) %>%
    dplyr::distinct()
}

# Check the site information looks alright
test_refuge_info <- function(refuge_info){
  multiple_rows_per_site <- refuge_info %>%
    has_single_row(refuge)

  if (multiple_rows_per_site)
    stop("Multiple rows per sampling in sammpling_info table")
}
