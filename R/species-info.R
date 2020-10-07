# Get a data frame with the species info for further use. Remove weird species
get_species_info <- function(fish_data_raw, species_to_exclude){

  fix_encoding <- function(x){
    x %>%
      `Encoding<-`("latin1") %>%
      utf8::utf8_normalize(., remove_ignorable = T)
  }

  fish_data_raw %>%
    dplyr::rename(species_code = speciescode) %>%
    dplyr::mutate_if(is.character, fix_encoding) %>%
    dplyr::mutate(species_name = snakecase::to_sentence_case(species_name)) %>%
    dplyr::filter(!stringr::str_detect(species_name, species_to_exclude)) %>%
    dplyr::select(species_name, species_code, family, genus, group, type) %>%
    dplyr::distinct() %>%
    # nice species id
    dplyr::mutate(species = make_id(species_code, "sp"))
}

# Test that the species info frame is fine
test_species_info <- function(species_info){
  multiple_rows_per_sp_code <- species_info %>%
    dplyr::count(species) %$%
    magrittr::is_greater_than(n, 1) %>%
    any()

  if (multiple_rows_per_sp_code)
    stop("Multiple rows per species code in species_info table")

  multiple_rows_per_sp_name <- species_info %>%
    dplyr::mutate(species_name = tolower(species_name)) %>%
    dplyr::count(species_code) %$%
    magrittr::is_greater_than(n, 1) %>%
    any()

  if (multiple_rows_per_sp_name)
    stop("Multiple rows per species name in species_info table")
}
