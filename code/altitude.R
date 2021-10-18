library(geonames)
library(glue)
options(geonamesUsername = "btree")
# Start and End Row we want to get elevation
rowStart <- 1
rowEnd <- nrow(data)

# Create a Named Vector with IDS
vIDs <- data$id[rowStart:rowEnd]
vValues <- setNames(rep(NA, length(vIDs)), vIDs)

# Loop with given limits
for (i in rowStart:rowEnd) {
    lID <- data$id[i] %>% as.character()
    lElevation <- GNsrtm3(lat = data[i, "latitude"], lng = data[i, "longitude"])$srtm3
    print(paste("ID:", lID, " Elevation: ", lElevation))
    vValues[lID] <- lElevation
    Sys.sleep(2) # we prevent overuse (1k : 1 hour)
}
print("------RESULTS--------")
vValues
print("---------------------")

vValues_drop <- vValues[!is.na(vValues)]
# save to csv
vValues_tibble <- as_tibble(vValues_drop) %>%
    tibble::add_column(id = data$id) # %>%
vValues_tibble %>% write_excel_csv2(
    file = glue("{here()}/raw/elevation.csv")
)


as_tibble(vValues) %>%
    tibble::add_column(id = names(vValues))

length(vValues[!is.na(vValues)])

vValues

dfCache %>% filter(id == "6657-20/21")
dfCache %>% filter(id == "7806-20/21")

dfCache <- dfCache %>%
    mutate(
        longitude = ifelse(id == "6657-20/21", 10.9016903, longitude)
    ) %>%
    mutate(
        longitude = ifelse(id == "7806-20/21", 14.8083347307326, longitude)
    )