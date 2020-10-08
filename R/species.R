# Get a data frame with the species info for further use. Remove weird species
get_species_info <- function(fish_data_raw, species_to_exclude){


  fish_data_raw %>%
    dplyr::select(species_name, species_code, family, genus, group, type) %>%
    dplyr::distinct()

}

# Test that the species info frame is fine
test_species_info <- function(species_info){

  multiple_rows_per_sp_code <- species_info %>%
    has_single_row(species)

  if (multiple_rows_per_sp_code)
    stop("Multiple rows per species code in species_info table")

  multiple_rows_per_sp_name <- species_info %>%
    has_single_row(species_name)

  if (multiple_rows_per_sp_name)
    stop("Multiple rows per species name in species_info table")
}
