# Get a data frame with the occasion dates for further use
get_ocassion_info <- function(fish_data_raw){
  fish_data_raw %>%
    dplyr::group_by(occasion) %>%
    dplyr::summarise(sampling_start = min(date),
                     sampling_end = max(date)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("sampling")),
                     ~ as.Date(., origin = "1899-12-30")) %>%
    dplyr::mutate(month = lubridate::month(sampling_start, label = T),
                  year = lubridate::year(sampling_start)) %>%
    # Nice occasion id
    dplyr::mutate(occasion = make_id(occasion, "oc"))
}

