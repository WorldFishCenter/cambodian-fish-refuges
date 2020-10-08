get_catch_info <- function(community_data){
  community_data %>%
    dplyr::select(refuge, occasion, replicate, gear, sampling, no_fish,
                  total_weight, species, min_length, max_length, min_weight,
                  max_weight, mean_weight)
}
