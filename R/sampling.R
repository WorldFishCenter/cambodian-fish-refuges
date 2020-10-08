# Get a data frame with the sampling info for further use
get_sampling_info <- function(community_data){
  community_data %>%
    dplyr::select(refuge, occasion, replicate, gear, sampling, gear_name,
                  net_length, net_area, depth_shore, depth_end) %>%
    dplyr::distinct()
}

test_sampling_info <- function(sampling_info){
  single_row_sampling <- sampling_info %>%
    has_single_row(sampling) %>%
    if (isFALSE(single_row_sampling)) {
      stop("Multiple rows per species name in species_info table")
    }
}
