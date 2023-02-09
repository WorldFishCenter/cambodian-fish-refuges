# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)
library(ggplot2)
library(ggtext)

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
  #refuge_info_OK = test_refuge_info(refuge_info),
  occasion_info = get_occasion_info(community_data),
  occasion_info_OK = test_occasion_info(occasion_info),
  sampling_info = get_sampling_info(community_data),
  species_info = get_species_info(community_data, n_fish_types),
  species_info_OK = test_species_info(species_info),
  catch_info = get_catch_info(community_data),
)

notebooks_plan <- drake_plan(
  sampling_bias_nb = target(rmarkdown::render(knitr_in("notebooks/sampling-bias.Rmd"))),
  catch_comp_nb = target(rmarkdown::render(knitr_in("notebooks/catch-composition.Rmd"))),
  seasonality_nb = target(rmarkdown::render(knitr_in("notebooks/seasonality.Rmd"))),
  correspondence_nb = target(rmarkdown::render(knitr_in("notebooks/correspondence-analysis.Rmd"))),
  diversity_nb = target(rmarkdown::render(knitr_in("notebooks/diversity.Rmd"))),
  beta_diversity_nb = target(rmarkdown::render(knitr_in("notebooks/beta-diversity-index.Rmd"))),
  refuge_nb = target(rmarkdown::render(knitr_in("notebooks/refuge-info.Rmd"))),
  readme = target(rmarkdown::render(knitr_in("README.Rmd"))),
)

analysis_plan <- drake_plan(
  species_totals_in_occasion = calc_species_totals_in_occasion(catch_info),
  occasion_covariates = get_occasion_covariates(occasion_info),
  refuge_covariates = get_refuge_covariates(refuge_info),
  model_alpha_diversity = fit_alpha_diversity_model(catch_info, occasion_covariates, refuge_covariates),
  model_dry_wet = fit_dry_wet_model(catch_info, occasion_covariates, refuge_covariates),
  catch_full = get_catch_full_info(catch_info, occasion_covariates),
  change_matrices = get_change_matrices(catch_full),
  tbi = calculate_tbi(change_matrices, catch_full),
  species_changes = get_species_changes(change_matrices, species_info),
  model_tbi_abu_comp = fit_tbi_abu_comp(tbi, refuge_covariates),
  occasion_covariates_beta = get_occasion_covariates_beta(occasion_info),
  refuge_covariates_beta = get_refuge_covariates_beta(refuge_info),
  tbi_model_data = get_tbi_model_data(tbi, occasion_covariates_beta, refuge_covariates_beta),
  tbi_model = model_tbi(tbi_model_data),
  species_change_model = model_species_changes(species_changes),
  # model_exploration = select_model_tbi(tbi, occasion_covariates_beta, refuge_covariates_beta),
  prelim_report = target(rmarkdown::render(knitr_in("notebooks/report.Rmd"))),
)


figures_plan <- drake_plan(
  tbi_category_plot = plot_tbi_category(model_tbi_abu_comp),
  target(ggsave("figures/fig_tbi-category.png", tbi_category_plot,
                width = 17/2, height = 7, units = "cm", dpi = 320)),
  species_groups_changes_plot = plot_species_groups_changes(species_change_model, species_changes),
  target(ggsave("figures/fig_sp-groups-changes.png", species_groups_changes_plot,
                width = 17/2, height = 2.4, units = "cm", dpi = 320)),
  tbi_factors_plot = plot_tbi_factors(tbi_model),
  target(ggsave("figures/fig-conditional-tbi.png", tbi_factors_plot,
                width = 17, height = 5.2, units = "cm", dpi = 320)),
  tbi_randomeffects_plot = plot_tbi_random_effects(model_tbi_abu_comp),
  target(ggsave(file_out("figures/fig-tbi-random2.png"), tbi_randomeffects_plot,
                width = 17, height = 17, units = "cm", dpi = 320))
)

full_plan <- rbind(script_variables,
                   data_preprocessing,
                   notebooks_plan,
                   analysis_plan,
                   figures_plan)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan, lock_envir = F, seed = 0)
}
