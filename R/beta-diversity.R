get_catch_full_info <- function(catch_info, occasion_covariates){
  require(tidyverse)

  catch_info %>%
    add_replicates_in_occasion(standard = TRUE) %>%
    # Convert to a matrix
    pivot_wider(id_cols = c("refuge", "occasion", "gear"),
                names_from = "species",
                values_from = "no_fish") %>%
    # fill un-sampled species to zero
    mutate(across(where(is_numeric), tidyr::replace_na, replace = 0)) %>%
    # Only Gill net
    filter(gear == "GN") %>%
    left_join(occasion_covariates) %>%
    select(!starts_with("sp"), starts_with("sp")) %>%
    group_by(year_s, season, month, refuge) %>%
    summarise(across(starts_with("sp"), sum), .groups = "drop") %>%
    # Make sure samplings with zero fish are included
    filter(!(refuge == "fr15" & year_s == 0),
           year_s < 3) %>%
    arrange(year_s, refuge)
}


get_change_matrices <- function(catch_full){
  catch_full %>%
    split(.$month) %>%
    map(~select(., -(year_s:refuge))) %>%
    map(as.matrix)
}

calculate_tbi <- function(change_matrices, catch_full){

  require(tidyverse)


  combinations <- list(
    `2.5` = c("2", "5"),
    `5.8` = c("5", "8"),
    `5.11` = c("5", "11")
  )

  type <-  list(presence_absence = TRUE, abundance = FALSE)

  tbi <- cross2(combinations, type) %>%
    map(function(x){
      adespatial::TBI(mat1 = change_matrices[[x[[1]][1]]],
                      mat2 = change_matrices[[x[[1]][2]]],
                      nperm = 99, test.BC = TRUE,
                      test.t.perm = T, save.BC = T,
                      method = "%difference", pa.tr = x[[2]])
    })

  type_combinations <- cross2(combinations, type) %>%
    map_df(~tibble(comparison = paste(.[[1]][1], .[[1]][2], collapse = "."),
                   pa_tr = .[[2]]), .id = "type")

  tbi %>%
    map_df(function(x){
      p <- x$TBI
      bc <- x$BC
      x$BCD.mat %>%
        as_tibble() %>%
        set_colnames(c("b_stnd", "c_stnd", "d", "change")) %>%
        select(-change) %>%
        mutate(p_value = p,
               b = bc[, 1],
               c = bc[, 2]) %>%
        bind_cols(distinct(select(catch_full, year_s, refuge)))

    }, .id = "type") %>%
    left_join(type_combinations, by = "type")
}

get_species_changes <- function(change_matrices, species_info){

  require(tidyverse)

  sp_tbi <- change_matrices %$%
    adespatial::tpaired.krandtest(mat1 = `5`, mat2 = `2`,
                                  nperm = 9999, list.all = T) %$%
    as.data.frame(t.tests) %>%
    rownames_to_column() %>%
    select(1:5) %>%
    set_colnames(c("species", "mean_diff", "t", "p_value_param", "p_value_perm")) %>%
    mutate(across(where(is.numeric), na_if, y = -999)) %>%
    left_join(species_info)

  sp_tbi %>%
    mutate(p_value_param_adjust = p.adjust(p_value_param))
}

fit_tbi_abu_comp <- function(tbi, refuge_covariates){

  require(tidyverse)
  require(brms)

  d <- tbi %>%
    filter(comparison == "2 5") %>%
    mutate(across(where(is.numeric), ~if_else(. == 0, 0.0001, .))) %>%
    left_join(refuge_covariates)

  brm(mvbind(b_stnd, c_stnd) ~ pa_tr + (1 |category_name / cfr_name) + (1 | year_s),
      data = d, family = Beta, cores = 4, iter = 5000,
      control = list(adapt_delta = 0.99))
}

get_occasion_covariates_beta <- function(occasion_info){

  require(tidyverse)

  oc <- occasion_info %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           season = if_else(month %in% c(2,5), "dry", "wet"),
           across(c("month", "year"), as.character)) %>%
    mutate(year_s = (as.numeric(substr(occasion, 3, 4))-1) %/% 4,
           month_s = paste0("m_", month)) %>%
    # Variables that will be included in the model
    select(occasion, refuge, year_s, month, season, aquatic_plant_area, gauge_start, water_temp, phosphate, brush_park, refuge, illegal_fishing, other_animal, water_bird) %>%
    distinct() %>%
    filter(season == "dry")

  oc_m2 <- oc %>%
    filter(month == "2")

  oc_m5 <- oc %>%
    filter(month == "5")

  full_join(oc_m2, oc_m5,
            by = c("refuge", "year_s"),
            suffix = c("_m2", "_m5")) %>%
    mutate(water_temp_diff = water_temp_m5 - water_temp_m2,
           gauge_start_diff = gauge_start_m5 - gauge_start_m2)
}

get_refuge_covariates_beta <- function(refuge_info){

  require(tidyverse)

  refuge_info %>%
    # Log transform those variables that need it
    mutate(across(c(area_dry_ha, area_wet_ha, conservation_ha, depth_dry_m, depth_wet_m, dist_channel_connect_cfr_rf_m, dist_market, dist_prov_capital, dist_village, lwb_area_dry_ha, lwb_area_wet_ha, lwb_depth_dry_m, lwb_depth_wet_m, rf_area_connected_in_dry_season_ha, rf_area_connected_in_wet_season_ha), log1p)) %>%
    # Select those that could be included in the model
    select(refuge,
           cfr_name,
           category_name,
           large_water_body,
           channel_type,
           shape,
           type_inlet_outlet,
           # numerical
           lwb_depth_dry_m,
           lwb_area_wet_ha,
           no_channel,
           rf_area_connected_in_dry_season_ha,
           dist_village,
           dist_market)
}


select_model_tbi <- function(tbi, occasion_covariates_beta, refuge_covariates_beta){

  require(glmulti)
  require(tidyverse)
  require(nlme)

  md <- tbi %>%
    left_join(refuge_covariates_beta) %>%
    left_join(occasion_covariates_beta) %>%
    mutate(across(c(b_stnd, c_stnd), ~if_else(. == 0, 0.001, .))) %>%
    mutate(across(c(b_stnd, c_stnd),qlogis)) %>%
    filter(comparison == "2 5" , !pa_tr)

  # formulas = list(b = formula(b_stnd ~ type_inlet_outlet + large_water_body +
  #                               channel_type + shape + lwb_depth_dry_m +
  #                               lwb_area_wet_ha + no_channel +
  #                               rf_area_connected_in_dry_season_ha + dist_market +
  #                               aquatic_plant_area_m2 + gauge_start_m2 +
  #                               water_temp_m2 + phosphate_m2 + brush_park_m2 +
  #                               illegal_fishing_m5 + other_animal_m2 +
  #                               water_bird_m2 + water_temp_diff + gauge_start_diff),
  #                 c = formula(c_stnd ~ type_inlet_outlet + large_water_body +
  #                               channel_type + shape + lwb_depth_dry_m +
  #                               lwb_area_wet_ha + no_channel +
  #                               rf_area_connected_in_dry_season_ha + dist_market +
  #                               aquatic_plant_area_m2 + gauge_start_m2 +
  #                               water_temp_m2 + phosphate_m2 + brush_park_m2 +
  #                               illegal_fishing_m5 + other_animal_m2 +
  #                               water_bird_m2 + water_temp_diff + gauge_start_diff))

  crr <- do.call("glmulti",
                 list(y = c_stnd ~ type_inlet_outlet + large_water_body +
                        channel_type + shape + lwb_depth_dry_m +
                        lwb_area_wet_ha + no_channel +
                        rf_area_connected_in_dry_season_ha + dist_market +
                        aquatic_plant_area_m2 + gauge_start_m2 +
                        water_temp_m2 + phosphate_m2 + brush_park_m2 +
                        illegal_fishing_m5 + other_animal_m2 +
                        water_bird_m2 + water_temp_diff + gauge_start_diff,
                      random = list(~ 1 | year_s),
                      data = md,
                      level = 1,
                      method = "g",
                      marginality = T,
                      fitfunction = "lme"))

  brr <- do.call("glmulti",
                 list(y = b_stnd ~ type_inlet_outlet + large_water_body +
                            channel_type + shape + lwb_depth_dry_m +
                        lwb_area_wet_ha + no_channel +
                        rf_area_connected_in_dry_season_ha + dist_market +
                        aquatic_plant_area_m2 + gauge_start_m2 +
                        water_temp_m2 + phosphate_m2 + brush_park_m2 +
                        illegal_fishing_m5 + other_animal_m2 +
                        water_bird_m2 + water_temp_diff + gauge_start_diff,
                      random = list(~ 1 | year_s),
                      data = md,
                      level = 1,
                      method = "g",
                      marginality = T,
                      fitfunction = "lme"))



  list(b = brr, c = crr)
}
