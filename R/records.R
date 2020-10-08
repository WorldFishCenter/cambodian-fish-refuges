

get_records <- function(fish_data_raw){
  fish_data_raw %>%
    dplyr::select(, crf_id, no, rec_id, replicate_no, occasion, gear, ) %>%
    dplyr::rename(refuge = crf_id,
                  sampling = no,
                  replicate = replicate_no) %>%
    dplyr::mutate(occasion = make_id(occasion, "oc")) %>%
    dplyr::mutate(refuge = make_id(refuge, "fr"))
}

get_casting <- function(fish_data_raw){
  fish_data_raw %>%
    dplyr::select(cfr_id, occasion, replicate_no, gear, gear_code, net_l, net_area, depth_shore,
                  depth_end) %>%
    dplyr::rename(net_length = net_l,
                  replicate = replicate_no,
                  refuge = cfr_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(occasion = make_id(occasion, "oc")) %>%
    dplyr::mutate(refuge = make_id(refuge, "fr"))
}

