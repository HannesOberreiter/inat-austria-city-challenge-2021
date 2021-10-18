library(tidyverse)
library(here)
library(readxl)
library(sf)
source("stats/misc.R")
# Import Data ---------
clean_names <- c(
    "id", "location", "district", "state", "country",
    "latitude", "longitude", "accuracy", "observed_date",
    "family", "species", "url", "comment", "quality", "downloaded_date", "observer", "identifier"
)
data <- readxl::read_xlsx(
    "inaturalist_export.xlsx",
    sheet = "heteroptera", col_names = clean_names, skip = 1
) %>%
    mutate(
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude)
    )
data %>% glimpse()

# Import Map --------
austria <- read_sf("map")
# Transform to WGS84
austria <- sf::st_transform(austria, crs = 4326)
austria_states <- austria %>%
    group_by(BL) %>%
    summarize(
        geometry = st_union(geometry)
    )
austria_states_simplify <- austria_states %>%
    st_simplify(dTolerance = 0.002)

austria_districts <- austria %>%
    group_by(PB) %>%
    summarize(
        geometry = st_union(geometry)
    )
austria_districts_simplify <- austria_districts %>%
    st_simplify(dTolerance = 0.002)

# Create SF Object
data_sf <- sf::st_as_sf(
    data,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
)

# Check States
check_state <- data_sf %>%
    sf::st_join(austria_states) # %>%
#   dplyr::add_count(BL) # %>%
# dplyr::distinct(BL, .keep_all = TRUE)

# nrow(check_state)
#check_state %>%
#    select(id, BL, state) %>%
#    sf::st_drop_geometry() %>%
#    write_excel_csv2("export.csv")
#View(check_state %>%
#    filter(BL != state))

# Check District
#get_district <- data_sf %>%
#    sf::st_join(austria_districts) %>%
#    select(id, PB) %>%
#    sf::st_drop_geometry() %>%
#    write_excel_csv2("export.csv")


# Plot our Species -----
species <- unique(data$species)
(species <- species %>% sort())

for (i in species) {
    print(i)
    data_sub <- data_sf %>% filter(species == i)
    # plotting heatmap counts
    map_plot <- austria_districts_simplify %>%
        sf::st_join(data_sub) %>%
        filter(!is.na(id)) %>%
        dplyr::add_count(PB) %>%
        group_by(PB) %>%
        filter(row_number() == 1) %>%
        ungroup()
    total_count <- sum(map_plot$n)
    p <- ggplot2::ggplot() +
        geom_sf(
            data = map_plot,
            aes(group = PB, fill = n),
            color = "black",
            size = 0.3
        ) +
        geom_sf(
            data = austria_states_simplify,
            aes(group = BL),
            inherit.aes = FALSE,
            alpha = 0,
            color = "black",
            size = 0.7
        ) +
        geom_point(
            data = data_sub,
            aes(x = longitude, y = latitude, color = "D55E00"),
            show.legend = FALSE
        ) +
        ggplot2::scale_fill_viridis_c() +
        coord_sf() +
        xlab("") +
        ylab("") +
        theme_classic() +
        ggtitle(glue::glue("{i} - Beobachtungen: {total_count}"))

    fSaveImages(p, i)
}
