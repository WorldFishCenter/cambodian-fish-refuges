get_top_species <- function(catch_full, species_info, threshold = 0.5){

  suppressPackageStartupMessages({
    require(tidyverse)
  })

  catch_full %>%
    pivot_longer(starts_with("sp"), names_to = "species", values_to = "count") %>%
    filter(season == "dry") %>%
    group_by(species) %>%
    summarise(total_count = sum(count)) %>%
    arrange(desc(total_count)) %>%
    ungroup() %>%
    mutate(perc_count = total_count / sum(total_count),
           cum_perc_count = cumsum(perc_count)) %>%
    filter(lag(cum_perc_count, default = 0) < threshold) %>%
    left_join(species_info, by = "species") %>%
    magrittr::extract2("species_name") %>%
    glue::glue_collapse(sep = ", ", last = ", and ")

}

species_table <- function(catch_full, species_changes, file_out){

  suppressPackageStartupMessages({
    require(tidyverse)
  })

  species_props <- catch_full %>%
    pivot_longer(starts_with("sp"), names_to = "species", values_to = "count") %>%
    filter(season == "dry") %>%
    group_by(species) %>%
    summarise(mean_count = mean(count),
              mean_count_pres = mean(count[count > 0]),
              perc_samplings = sum(count> 0)/n()) %>%
    arrange(desc(mean_count)) %>%
    mutate(top = 1:n(),
           top = top <= 10)

  species_changes %>%
    full_join(species_props) %>%
    as_tibble() %>%
    mutate(p_value_perm_adjust = p.adjust(p_value_perm),
           type = paste(type, "\nspecies"),
           perm_larger = p_value_perm >= p_value_param,
           perc_change = (1 - exp(mean_diff))*-1) %>%
    filter(p_value_perm <= 0.05 | top) %>%
    select(species_name, species_type_simple, perc_change, p_value_perm,
           p_value_perm_adjust, mean_count, perc_samplings, top) %>%
    arrange(p_value_perm, p_value_perm_adjust) %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    mutate_at(vars(starts_with("p_v")), format, trip = F) %>%
    mutate_at(vars(starts_with("p_v")), ~if_else(. == "0.000", "<0.001", .)) %>%
    mutate_at(vars(starts_with("perc")), scales::percent, accuracy = 0.1, trim = FALSE) %>%
    mutate_at(vars(starts_with("mean")), scales::number, accuracy = 0.1, trim = FALSE)  %>%
    write_csv(file_out)

}
