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

  suppressPackageStartupMessages({
    library(tidyverse)
  })

  catch_full %>%
    split(.$month) %>%
    map(~select(., -(year_s:refuge))) %>%
    map(as.matrix) %>%
    map(log1p)
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
                      nperm = 9999, test.BC = TRUE,
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

  brm(mvbind(b_stnd, c_stnd, d) ~ pa_tr*category_name + (1 | cfr_name) + (1 | year_s),
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
           no_inlet_outlet,
           # numerical
           lwb_depth_dry_m,
           lwb_area_wet_ha,
           no_channel,
           rf_area_connected_in_dry_season_ha,
           rf_area_connected_in_wet_season_ha,
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
    mutate(across(c(b_stnd, c_stnd, d), ~if_else(. == 0, 0.001, .))) %>%
    mutate(across(c(b_stnd, c_stnd, d), qlogis)) %>%
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

  drr <- do.call("glmulti",
                 list(y = d ~ type_inlet_outlet + large_water_body +
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

  list(b = brr, c = crr, d = drr)
}


get_tbi_model_data <- function(tbi, occasion_covariates_beta, refuge_covariates_beta){

  suppressPackageStartupMessages({
    require(tidyverse)
  })

  tbi %>%
    left_join(refuge_covariates_beta) %>%
    left_join(occasion_covariates_beta) %>%
    filter(comparison == "2 5") %>%
    mutate(across(c(where(is.numeric) & !b_stnd:year_s), scale),
           across(where(is.character), as.factor),
           across(where( ~ is.factor(.x) && "None" %in% levels(.x)), ~fct_relevel(., "None")),
           across(where(is.logical), ~ if_else(.x, "yes", "no")))
}

model_tbi <- function(tbi_model_data){

  suppressPackageStartupMessages({
    require(brms)
  })

  # tbi_model_data <- tbi_model_data %>%
    # dplyr::mutate(no_inlet_outlet = as.character(no_inlet_outlet))
  #
  # mg <- gam(b_stnd ~ s(aquatic_plant_area_m2) +
  #       s(gauge_start_m5) +
  #       s(gauge_start_diff) +
  #       dist_village +
  #       s(rf_area_connected_in_wet_season_ha) +
  #       no_inlet_outlet +
  #       type_inlet_outlet +
  #       s(lwb_area_wet_ha),
  #     data = dplyr::filter(tbi_model_data, pa_tr == "no"))

  m1 <- brm(mvbind(b_stnd, c_stnd) ~
              aquatic_plant_area_m2 +
              gauge_start_m5 +
              gauge_start_diff +
              dist_village +
              rf_area_connected_in_wet_season_ha +
              # no_inlet_outlet +
              type_inlet_outlet +
              lwb_area_wet_ha +
              (1 | year_s) +
              (1 | refuge),
            family = "zero_inflated_beta",
            data = dplyr::filter(tbi_model_data, pa_tr == "no"),
            cores = 4,
            iter = 5000,
            control = list(adapt_delta = 0.99))

  m2 <- brm(mvbind(b_stnd, c_stnd) ~
              aquatic_plant_area_m2 +
              gauge_start_m5 +
              gauge_start_diff +
              dist_village +
              rf_area_connected_in_wet_season_ha +
              # no_inlet_outlet +
              type_inlet_outlet +
              lwb_area_wet_ha +
              (1 | year_s) +
              (1 | refuge),
            family = "zero_inflated_beta",
            data = dplyr::filter(tbi_model_data, pa_tr == "yes"),
            cores = 4,
            iter = 5000,
            control = list(adapt_delta = 0.99))

  list(m1, m2)

}


model_species_changes <- function(species_changes){

  suppressPackageStartupMessages({
    require(brms)
  })

  smd <- species_changes %>%
    dplyr::mutate(mean_diff = scale(mean_diff))

  m <- brm(t ~ species_type_simple + (1 | species),
           # family = student,
           data = smd,
           cores = 4,
           iter = 5000,
           control = list(adapt_delta = 0.99))

  m2 <- brm(mean_diff ~ species_type_simple + (1 | species),
           # family = student,
           data = smd,
           cores = 4,
           iter = 5000,
           control = list(adapt_delta = 0.99))

  list(t_values = m, mean_diff = m2)


}

shorten_category_name <- function(category_name, extra = FALSE){
  short_name <- case_when(
    category_name ==
      "Reservoir for irrigation in upland area" ~
      "(1) Irrigation reservoir",
    category_name ==
      "Community pond within agricultural land not prone to flood" ~
      "(2) Not-flooding pond",
    category_name ==
      "Community pond within agricultural land prone to flood" ~
      "(3) Flooding pond",
    category_name ==
      "Demarcated area in larger water body" ~ "(4) Large water body")

  if (extra) short_name <- stringr::str_extract(short_name, "[1-4]")

  short_name
}

lengthen_difference_type <- function(.category){
  case_when(
    .category == "bstnd" ~
      "Dissimilarity  due  to  **losses** in per-species  abundance",
    .category == "cstnd" ~
      "Dissimilarity due to **gains** in per-species abundance",
    TRUE ~
      "**Total** dissimilarity")
}

plot_tbi_category <- function(model_tbi_abu_comp){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidybayes)
    library(ggdist)
    library(ggtext)
  })

  tbi_pred_data <- tibble(
    category_name = unique(model_tbi_abu_comp$data$category_name),
    pa_tr = FALSE
  )

  tbi_predictions <- tbi_pred_data %>%
    add_fitted_draws(model_tbi_abu_comp, re_formula = NA)

  tbi_predictions %>%
    mutate(category_name = shorten_category_name(category_name)) %>%
    group_by(category_name, pa_tr, .draw) %>%
    mutate(total_value = .value[.category == "d"]) %>%
    ungroup() %>%
    mutate(category_name = fct_reorder(category_name, total_value, .fun = mean),
           .category = lengthen_difference_type(.category)) %>%
    ggplot(aes(x = .value, y = category_name, shape = .category, colour = .category)) +
    stat_gradientinterval(.width = c(.66, .95),
                          point_size = 1,
                          interval_size_domain = c(0.5,12),
                          interval_size_range = c(0.25, 1),
                          position = position_dodge(width = 1),
                          point_interval = mean_qi) +
    facet_wrap(.category ~ ., scales = "free_x", ncol = 1)  +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_color_manual(values = c("black", "#1f78b4", "#33a02c"), aesthetics = c("colour", "fill")) +
    scale_shape_manual(values = c(16, 17, 15))+
    coord_cartesian(xlim = c(0,1)) +
    theme_minimal(base_size = 8) +
    theme(axis.title.y = element_blank(),
          # panel.grid.major.y = element_blank(),
          legend.position = "none",
          legend.title = element_blank(),
          axis.title = element_text(size = 7),
          strip.text = element_markdown()) +
    labs(x = "Mean % difference")
 }


plot_species_groups_changes <- function(species_change_model, species_changes){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidybayes)
    library(ggdist)
    library(ggtext)
  })

  msc <- species_change_model$t_values

  labels <- tibble(
    x = 0, y = 0.1,
    label = c("  → gains   ", "   losses ←  "),
    hjust = c(0, 1)
  )

  add_fitted_draws(tibble(species_type_simple = unique(msc$data$species_type_simple)),
                                          msc, re_formula = NA, n = 999) %>%
    ungroup() %>%
    mutate(species_type_simple = fct_reorder(species_type_simple, .value)) %>%
    ggplot(aes(x = .value, y = species_type_simple)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    geom_point(data = species_changes, aes(x = t), shape = 124, size = 1) +
    stat_gradientinterval(.width = c(.66, .95),
                          point_size = 1,
                          interval_size_domain = c(0.5,12),
                          interval_size_range = c(0.25, 1),
                          shape = 16,
                          point_interval = mean_qi) +
    geom_text(data = labels, aes(label = label, y = y, x = x, hjust = hjust),
              fontface = "italic", size = 2.3) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 8) +
    theme(axis.title = element_text(size = 7),
          axis.title.y = element_blank()) +
    labs(x = "t-value",
         y = "Species type")

}

plot_tbi_factors <- function(tbi_model){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidybayes)
    library(ggdist)
    library(ggtext)
    library(patchwork)
  })

  tbi_model_abu <- tbi_model[[1]]

  inlet_type_df <- tibble(type_inlet_outlet = unique(tbi_model_abu$data$type_inlet_outlet),
                          aquatic_plant_area_m2 = 0,
                          gauge_start_m5 = 0,
                          gauge_start_diff = 0,
                          dist_village = 0,
                          rf_area_connected_in_wet_season_ha = 0,
                          no_inlet_outlet = 0,
                          lwb_area_wet_ha = 0)

  p1 <- add_fitted_draws(inlet_type_df, tbi_model_abu, re_formula = ~ 1) %>%
    group_by(type_inlet_outlet, .draw) %>%
    mutate(loss_value = .value[.category == "bstnd"]) %>%
    ungroup() %>%
    mutate(.category = lengthen_difference_type(.category),
           type_inlet_outlet = fct_recode(type_inlet_outlet,
                                          "No channel" = "None",
                                          "Earth channel" = "Earth"),
           type_inlet_outlet = fct_reorder(type_inlet_outlet, loss_value, mean)) %>%
    ggplot(aes(x = .value, y = type_inlet_outlet, shape = .category,
               colour = .category)) +
    stat_gradientinterval(position = position_dodge(width = 0.9),
                          .width = c(.66, .95),
                          point_size = 1,
                          interval_size_domain = c(0.5,12),
                          interval_size_range = c(0.25, 1),
                          point_interval = mean_qi) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_shape_manual(values = c(17, 15))+
    coord_cartesian(xlim = c(0.05,0.75)) +
    theme_minimal(base_size = 8) +
    theme(axis.title = element_text(size = 7),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_markdown(),
          legend.position = "bottom") +
    labs(x = "% difference",
         y = "Channel type")

  lwb_area_df <- tibble(type_inlet_outlet = "Water Gate",
                        aquatic_plant_area_m2 = 0,
                        gauge_start_m5 = 0,
                        gauge_start_diff = 0,
                        dist_village = 0,
                        rf_area_connected_in_wet_season_ha = 0,
                        no_inlet_outlet = 0,
                        lwb_area_wet_ha = unique(tbi_model_abu$data$lwb_area_wet_ha))

  lwb_area_df_fitted <- add_fitted_draws(lwb_area_df, tbi_model_abu, re_formula = ~ 1, n = 999) %>%
    mutate(area = lwb_area_wet_ha *
             attr(tbi_model_abu$data$lwb_area_wet_ha, "scaled:scale"),
           area = area +
             attr(tbi_model_abu$data$lwb_area_wet_ha, "scaled:center"))

  p2 <- lwb_area_df_fitted %>%
    filter(.category == "bstnd") %>%
    ggplot(aes(y = .value, x = area, fill = .category, colour = .category)) +
    stat_lineribbon(aes(alpha = stat(level)),
                    .width = c(0.05, 0.66, 0.95),
                    colour = NA,
                    point_interval = mean_qi) +
    geom_point(data = tibble(area = unique(lwb_area_df_fitted$area[,1])),
              aes(y = -Inf, x = area), inherit.aes = F, shape = 124, size = 1) +
    theme_minimal(base_size = 8) +
    theme(axis.title = element_text(size = 7),
          legend.position = "none") +
    scale_x_continuous(breaks = log1p(c(1, 10, 100, 1000)),
                       labels = expm1) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_alpha_discrete(range = c(0.1, 1)) +
    coord_cartesian(ylim = c(0.1, 0.7)) +
    labs(x = "Water-body area (ha)",
         y = "% difference")

  rf_area_df <- tibble(type_inlet_outlet = "Water Gate",
                        aquatic_plant_area_m2 = 0,
                        gauge_start_m5 = 0,
                        gauge_start_diff = 0,
                        dist_village = 0,
                        rf_area_connected_in_wet_season_ha = unique(tbi_model_abu$data$rf_area_connected_in_wet_season_ha),
                        no_inlet_outlet = 0,
                        lwb_area_wet_ha = 0)

  rf_area_df_fitted <- add_fitted_draws(rf_area_df, tbi_model_abu, re_formula = ~ 1, n = 999) %>%
    mutate(area = rf_area_connected_in_wet_season_ha *
             attr(tbi_model_abu$data$rf_area_connected_in_wet_season_ha, "scaled:scale"),
           area = area +
             attr(tbi_model_abu$data$rf_area_connected_in_wet_season_ha, "scaled:center"))

  p3 <- rf_area_df_fitted %>%
    filter(.category == "bstnd") %>%
    ggplot(aes(y = .value, x = area, fill = .category, colour = .category)) +
    stat_lineribbon(aes(alpha = stat(level)),
                    .width = c(0.05, 0.66, 0.95),
                    colour = NA,
                    point_interval = mean_qi) +
    geom_point(data = tibble(area = unique(rf_area_df_fitted$area[,1])),
               aes(y = -Inf, x = area), inherit.aes = F, shape = 124, size = 1) +
    theme_minimal(base_size = 8) +
    theme(axis.title = element_text(size = 7),
          legend.position = "none") +
    scale_x_continuous(breaks = log1p(c(0, 10, 100, 250, 500, 1000, 2000, 4000)),
                       labels = expm1) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_alpha_discrete(range = c(0.1, 1)) +
    coord_cartesian(ylim = c(0.1, 0.7)) +
    labs(x = "Rice field area connected\nduring wet season (ha)",
         y = "% difference")

  p <- ((p1 + p2 + p3) / guide_area() +
      plot_annotation(tag_levels = 'A', tag_suffix = ".") +
      plot_layout(guides = "collect", heights = c(10,1))) &
    scale_color_manual(values = c("#1f78b4", "#33a02c"), aesthetics = c("colour", "fill")) &
    theme(plot.tag = element_text(size = 7))

  return(p)
}

# lengthen_variacble_names <- fucntion

plot_tbi_effects <- function(tbi_model){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidybayes)
    library(ggdist)
    library(ggtext)
    library(patchwork)
  })

  tbi_model_abu <- tbi_model[[1]]

  find_base_level <- function(variable, level, data){
    if(is.na(level)) return(NA)

    data[, as.character(variable)] %>%
      as.factor() %>%
      levels() %>%
      first()
  }

  find_pd_position <- function(value, upper, lower){
    range <- c(lower, upper)
    same_side <- range[sign(range) == sign(value)]
    same_side[which.max(abs(same_side))]
  }

  effects_df <- tbi_model_abu %>%
    gather_draws(`b_.*`,  regex = T, n = 999) %>%
    mean_qi(.width = c(0.66, 0.95)) %>%
    ungroup() %>%
    filter(str_detect(.variable, "Intercept", negate = T)) %>%
    mutate(response = str_extract(.variable, "^b_([a-z]+)_"),
           level = str_extract(.variable, "[A-Z].+$"),
           variable = str_remove(.variable, response),
           variable = if_else(is.na(level),
                              variable,
                              str_remove(variable, level)),
           variable = fct_reorder(variable, abs(.value), .desc = F),
           level = fct_reorder(level, abs(.value))) %>%
    group_by(.variable, response) %>%
    rowwise() %>%
    mutate(base_level = find_base_level(variable, level, tbi_model_abu$data),
           var_level = if_else(is.na(level),
                               as.character(variable),
                               paste0(variable, " (", level, " vs. ", base_level, ")")),
           response = str_replace(response, "^b_([a-z]+)_", "\\1"),
           response = lengthen_difference_type(response),
           categorical = !is.na(base_level))

  p1 <- effects_df %>%
    filter(categorical) %>%
    ggplot(aes(x = .value, y = var_level, shape = response, colour = response)) +
    geom_vline(xintercept = 0, linetype = 2, width = 0.25) +
    geom_pointinterval(aes(xmin = .lower, xmax = .upper),
                       position = position_dodge(width = 0.75),
                       point_size = 1,
                       interval_size_domain = c(0.5,12),
                       interval_size_range = c(0.25, 1)) +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    theme_minimal(base_size = 8) +
    theme(legend.position = "none",
          axis.title.y = element_blank()) +
    labs(x = "Effect size categorical variables")

  p2 <- effects_df %>%
    filter(!categorical) %>%
    ggplot(aes(x = .value, y = variable, shape = response, colour = response)) +
    geom_vline(xintercept = 0, linetype = 2, width = 0.25) +

    geom_pointinterval(aes(xmin = .lower, xmax = .upper),
                       position = position_dodge(width = 0.75),
                       point_size = 1,
                       interval_size_domain = c(0.5,12),
                       interval_size_range = c(0.25, 1)) +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_markdown(),
          legend.direction = "vertical",
          axis.title.y = element_blank()) +
    labs(x = "Effect size continuous variables")

  (p1 + p2 + plot_layout(ncol = 1, heights = c(3, 7))) &
    scale_shape_manual(values = c(17, 15)) &
    scale_color_manual(values = c("#1f78b4", "#33a02c"), aesthetics = c("colour", "fill"))

  ggsave("figures/tbi-effect-sizes.png", width = 17, height = 10, units = "cm", dpi = 300)
}
