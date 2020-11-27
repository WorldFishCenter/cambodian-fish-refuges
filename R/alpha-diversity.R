# Add together number of fish and biomass collected over several replicates
add_replicates_in_occasion <- function(catch_info, standard = TRUE){

  require(dplyr)

  max_samplings_refuge <- catch_info %>%
    filter(gear == "GN") %>%
    group_by(refuge, occasion) %>%
    summarise(n_samples = n_distinct(replicate), .groups = "drop") %>%
    group_by(refuge) %>%
    summarise(n_samples = max(n_samples), .groups = "drop")

  replicates_long <- catch_info %>%
    filter(gear == "GN") %>%
    # standardise replicate sampling
    group_by(refuge, occasion, gear, species) %>%
    summarise(across(c(no_fish, total_weight), ~ sum(., na.rm = T)),
              .groups = "drop") %>%
    inner_join(max_samplings_refuge, by = "refuge")

  if (isTRUE(standard)) {
    replicates_long %>%
      mutate(across(c(no_fish, total_weight), ~ round(. / n_samples * 8)))
  } else {
    replicates_long
  }
}

calc_species_totals_in_occasion <- function(catch_info){
  list(standard_totals = TRUE, raw_totals = FALSE) %>%
    purrr::map_dfr(~ add_replicates_in_occasion(catch_info, .), .id = "standard")
}

get_diversity <- function(replicates_long,
                          values_from = "no_fish",
                          transform_fun = "I",
                          index = "shannon",
                          base = exp(1)){

  require(dplyr)
  require(tidyr)

  max_samplings_refuge <- replicates_long %>%
    filter(gear == "GN") %>%
    group_by(refuge, occasion) %>%
    summarise(n_samples = max(n_samples), .groups = "drop")

  replicates_wide <- replicates_long %>%
    pivot_wider(id_cols = c("refuge", "occasion", "gear"),
                names_from = "species",
                values_from = all_of(values_from)) %>%
  mutate(across(where(is.numeric), tidyr::replace_na, replace = 0),
         across(where(is.numeric), get(transform_fun))) %>%
    select(!starts_with("sp"), starts_with("sp"))

  div <- replicates_wide %>%
    select(starts_with("sp")) %>%
    as.matrix() %>%
    vegan::diversity(index = index, base = base)

  replicates_wide %>%
    select(!starts_with("sp")) %>%
    mutate(diversity_alpha = div,
           index = index,
           base = base,
           transform_fun = transform_fun) %>%
    left_join(max_samplings_refuge)
}

get_occasion_covariates <- function(occasion_info){

  require(dplyr)

  occasion_info %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           season = if_else(month %in% c(2,5), "dry", "wet"),
           water_level = (gauge_start + gauge_finish) / 2,
           visibility = secchi_depth,
           aquatic_plant_index = aquatic_plant_area * aquatic_plant_density,
           across(c("month", "year"), as.character)) %>%
    select(refuge, occasion, year, season, month, water_level, water_temp, visibility,
           aquatic_plant_area, aquatic_plant_index, brush_park) %>%
    distinct() %>%
    mutate(year_s = (as.numeric(substr(occasion, 3, 4))-1) %/% 4,
                     month_s = paste0("m_", month))
}

get_refuge_covariates <- function(refuge_info){

  require(dplyr)

  refuge_info %>%
    mutate(dry_area = area_dry_ha,
           dry_depth = depth_dry_m,
           rf_area_connected = log(rf_area_connected_in_wet_season_ha),
           across(category, as.character)) %>%
    select(refuge, category, cfr_name, category_name, dry_area, dry_depth, large_water_body)
}

compare_diversity_models <- function(){
  require(brms)

  drake::loadd(catch_info)
  drake::loadd(occasion_covariates)
  drake::loadd(refuge_covariates)

  # Here we informally try multiple models to see what might be the best
  # modelling approach

  # First we compare wether is better to log transform the abundance numbers.
  # Both bayes R2 and WAIC suggest that it might be better to transform

  m0 <- brm(formula = brmsformula(diversity_alpha ~
                              month_s +
                              (1 |category_name / cfr_name) +
                              (1 | year_s)),
      data = catch_info %>%
        add_replicates_in_occasion() %>%
        get_diversity() %>%
        left_join(occasion_covariates) %>%
        left_join(refuge_covariates))

  m1 <- brm(formula = brmsformula(diversity_alpha ~
                                    month_s +
                                    (1 |category_name / cfr_name) +
                                    (1 | year_s)),
            data = catch_info %>%
              add_replicates_in_occasion() %>%
              get_diversity(transform_fun = "log1p") %>%
              left_join(occasion_covariates) %>%
              left_join(refuge_covariates))

  # Then we check whether is better to standardise the number of species or
  # wether is better to use offsets in the model. We also check whether log
  # offsets are better than normal offsets. Log offsets are the better option

  m2 <- brm(formula = brmsformula(diversity_alpha ~
                                    month_s +
                                    offset(n_samples) +
                                    (1 |category_name / cfr_name) +
                                    (1 | year_s)),
            data = catch_info %>%
              add_replicates_in_occasion(standard = FALSE) %>%
              get_diversity(transform_fun = "log1p") %>%
              left_join(occasion_covariates) %>%
              left_join(refuge_covariates))

  m3 <- brm(formula = brmsformula(diversity_alpha ~
                                    month_s +
                                    offset(log(n_samples)) +
                                    (1 |category_name / cfr_name) +
                                    (1 | year_s)),
            data = catch_info %>%
              add_replicates_in_occasion(standard = FALSE) %>%
              get_diversity(transform_fun = "log1p") %>%
              left_join(occasion_covariates) %>%
              left_join(refuge_covariates))

  # Finally we check wether it matters if we consider years that start at the
  # end of the wet season (to focus on the dry season) or calendar years.
  # "flooding years" seem to be better than calendar years

  m4 <- brm(formula = brmsformula(diversity_alpha ~
                                    month_s +
                                    offset(log(n_samples)) +
                                    (1 |category_name / cfr_name) +
                                    (1 | year)),
            data = catch_info %>%
              add_replicates_in_occasion(standard = FALSE) %>%
              get_diversity(transform_fun = "log1p") %>%
              left_join(occasion_covariates) %>%
              left_join(refuge_covariates))
}

fit_alpha_diversity_model <- function(catch_info, occasion_covariates, refuge_covariates){

  require(dplyr)
  require(brms)

  model_data <- catch_info %>%
    add_replicates_in_occasion(standard = FALSE) %>%
    get_diversity(transform_fun = "log1p") %>%
    left_join(occasion_covariates) %>%
    left_join(refuge_covariates)

  brm(formula = brmsformula(diversity_alpha ~
                              month_s +
                              offset(log(n_samples)) +
                              (1 |category_name / cfr_name) +
                              (month_s | year_s)),
      data = model_data,
      prior = c(prior(normal(1, 5), class = Intercept),
                prior(normal(0, 3), class = b)),
      iter = 5000,
      cores = 4,
      save_pars = save_pars(all = TRUE))
}
