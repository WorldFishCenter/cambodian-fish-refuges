
# No: Sample number
# Replicate: 8 within each sample/ocassion/method
# Method: Fike net -- Gill net
# Net area: This is the important one
# Total hours: Total amount of time that took to complete the sampling on an ocassion
# day moon: phase of the moon?
# RFWaterLevel: Water level in the rice fields
# category: CRF category
# category_1:
# aquatic_plant_area: area covered by floating plants
# brush_park: no idea
# agro-eco-zone: Agricultural classification


clean_community_data <- function(community_data_raw, species_to_exclude){
  community_data_raw %>%
    janitor::clean_names() %>%
    # category_seems to be a bit weird
    dplyr::select(-category) %>%
    dplyr::rename(category = category_1) %>%
    # Funny column names
    dplyr::rename(fia_designated = fi_a_designated,
                  large_water_body = large_wb_yes_1_no_2,
                  channel_type = channel_1_earth_2_concrete_3_both,
                  sampling = no,
                  refuge = cfr_id,
                  water_temp = water_tem,
                  illegal_fishing = illegal_fishing_seen,
                  net_length = net_l,
                  replicate = replicate_no,
                  gear_name = gear,
                  gear = gear_code,
                  species_code = speciescode) %>%
    # Parse date
    dplyr::mutate(date = as.Date(date, origin = "1899-12-30")) %>%
    # Encoding of characters
    dplyr::mutate_if(is.character, fix_encoding) %>%
    # Site info column contents
    dplyr::mutate(channel_type = dplyr::case_when(channel_type == 1 ~ "earth",
                                                  channel_type == 2 ~ "concrete",
                                                  channel_type == 3 ~ "both",
                                                  TRUE ~ NA_character_),
                  large_water_body = large_water_body == 1) %>%
    # Occasion info column contents
    dplyr::mutate_at(dplyr::vars("illegal_fishing",
                                 "water_bird",
                                 "other_animal",
                                 "weather"), tolower) %>%
    dplyr::mutate_at(dplyr::vars("illegal_fishing",
                                 "water_bird",
                                 "other_animal"), ~ !. == "no") %>%
    # Species names
    dplyr::mutate(species_name = snakecase::to_sentence_case(species_name)) %>%
    dplyr::filter(!stringr::str_detect(species_name, species_to_exclude)) %>%
    # Make id columns better
    dplyr::mutate(refuge = make_id(refuge, "fr")) %>%
    dplyr::mutate(occasion = make_id(occasion, "oc")) %>%
    dplyr::mutate(replicate = make_id(replicate, "re")) %>%
    dplyr::mutate(species = make_id(species_code, "sp")) %>%
    dplyr::mutate(refuge_occasion = paste(refuge, occasion, sep = "_")) %>%
    dplyr::mutate(sampling = paste(refuge, occasion, replicate,
                                   gear, sep = "_"))
}

