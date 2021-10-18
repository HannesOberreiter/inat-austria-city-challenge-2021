library(tidyverse)
library(tidygeocoder)

# Google API --------------------------------------------------------------
geo_result <- data %>%
    # filter(id == "1602961") %>%
    tidygeocoder::reverse_geocode(
        lat = "latitude",
        long = "longitude",
        method = "google",
        limit = 1,
        no_query = TRUE
    )

write_excel_csv(geo_result, "export_coords.csv")