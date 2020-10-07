# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)

# Plan analysis ------------------------------------------------------------

script_variables <- drake_plan(
  species_to_exclude = c("Unname"),
  fish_data_path = file_in("data-raw/cambodia.csv")
)

data_preprocessing <- drake_plan(
  fish_data_raw = clean_fish_data(fish_data_path),
  occasion_info = get_ocassion_info(fish_data_raw),
  species_info = get_species_info(fish_data_raw, species_to_exclude),
  species_info_OK = test_species_info(species_info),
  sampling_info = get_sampling_info(fish_data_raw),
  sampling_info_OK = test_sampling_info(sampling_info),

)

full_plan <- rbind(script_variables,
                   data_preprocessing)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan)
}
