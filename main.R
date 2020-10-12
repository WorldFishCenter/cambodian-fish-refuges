# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)

# Plan analysis ------------------------------------------------------------

script_variables <- drake_plan(
  species_to_exclude = c("Unname"),
  community_data_path = file_in("data-raw/cambodia.csv"),
  n_fish_types = 3,
)

data_preprocessing <- drake_plan(
  community_data_raw = readr::read_csv(file = community_data_path,
                                  na = c("", "NA", "#VALUE!")),
  community_data = clean_community_data(community_data_raw, species_to_exclude),
  refuge_info = get_refuge_info(community_data),
  refuge_info_OK = test_refuge_info(refuge_info),
  occasion_info = get_occasion_info(community_data),
  occasion_info_OK = test_occasion_info(occasion_info),
  sampling_info = get_sampling_info(community_data),
  species_info = get_species_info(community_data, n_fish_types),
  species_info_OK = test_species_info(species_info),
  catch_info = get_catch_info(community_data),
)

notebooks_plan <- drake_plan(
  sampling_bias_nb = target(rmarkdown::render(knitr_in("notebooks/sampling-bias.Rmd"))),
  catch_comp_nb = target(rmarkdown::render(knitr_in("notebooks/catch-composition.Rmd")))
)

full_plan <- rbind(script_variables,
                   data_preprocessing,
                   notebooks_plan)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan)
}
