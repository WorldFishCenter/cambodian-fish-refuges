clean_fish_data <- function(path){
  readr::read_csv(file = path,
                  na = c("", "NA", "#VALUE!")) %>%
    janitor::clean_names()
}


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
