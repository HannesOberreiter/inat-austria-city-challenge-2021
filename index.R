# Header ----
library(tidyverse)
library(here)
library(jsonlite)
library(lubridate)
library(scales)
library(rinat)
library(sf)
library(geojsonsf)

source("functions/functions.R")

# Obersations Total ----
g_url <- "https://api.inaturalist.org/v1/observations/histogram?date_field=observed&interval=year"
a_url <- "https://api.inaturalist.org/v1/observations/histogram?date_field=observed&interval=year&place_id=143465"
g_fetch_url <- fromJSON(g_url)
a_fetch_url <- fromJSON(a_url)
obs_df <- bind_rows(g_fetch_url$results) %>%
    pivot_longer(everything()) %>%
    left_join(
        bind_rows(a_fetch_url$results) %>%
            pivot_longer(everything()),
        by = c("name")
    ) %>%
    rename(global = value.x, austria = value.y) %>%
    mutate(
        date = lubridate::as_date(name),
        year = lubridate::year(date),
        year = ifelse(year < 2008, 2008, year)
    ) %>%
    group_by(year) %>%
    summarise(
        global = sum(global, na.rm = TRUE),
        austria = sum(austria, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        global = cumsum(global),
        austria = cumsum(austria)
    ) %>%
    pivot_longer(-year) %>%
    group_by(name) %>%
    mutate(
        name = ifelse(name == "global", "Weltweit", "Österreich"),
        latest = dplyr::last(value) %>% format(big.mark = ".", decimal.mark = ","),
        label = glue::glue("{name} - {latest} Beobachtungen")
    ) %>%
    glimpse()


p <- fTotalLine(obs_df, "Beobachtungen [#]")
fSaveImages(p, "Observations_Total")

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

map_grid <- st_make_grid(
    austria_states_simplify,
    cellsize = 0.1
)

# https://github.com/r-spatial/sf/issues/243
map_grid <- map_grid[austria_states_simplify] %>%
    st_sf() %>%
    st_cast() %>%
    mutate(id_polygon = row_number())

# Austria Observations ----
gbif_data <- readr::read_tsv("data/0031008-210914110416597.csv")
# Create SF Object
gbif_sf <- sf::st_as_sf(
    gbif_data,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326,
    remove = FALSE
)
# Counting Observations inside Polygons
# gbif_polygon <- map_grid %>%
# sf::st_join(gbif_sf)
count <- st_intersects(map_grid, gbif_sf, sparse = FALSE)
count <- apply(r, 1, sum)
map_grid$count <- count

p <- ggplot2::ggplot() +
    geom_sf(
        data = map_grid,
        aes(fill = count),
    ) +
    geom_sf(
        data = map_grid %>% filter(count == 0),
        # aes(fill = count),
        fill = "white"
    ) +
    geom_sf(
        data = austria_states_simplify,
        aes(group = BL),
        inherit.aes = FALSE,
        alpha = 0,
        color = "#CC79A7",
        size = 0.7
    ) +
    scale_fill_viridis_c(
        # option = "inferno",
        direction = -1,
        breaks = c(1, 100, 200, 300),
        labels = c("1", "100", "200", "> 300"),
        limits = c(1, 300),
        oob = scales::squish,
    ) +
    coord_sf() +
    labs(x = "", y = "", fill = "Research Grade Beobachtungen:") +
    theme_void() +
    theme(
        legend.position = "bottom"
    )
fSaveImages(p, "Map_GBIF_Obs")

# Load City Challenge Data ----
data <- readxl::read_xlsx(
    "data/16-10-2021-edited.xlsx",
    col_types = "text",
    sheet = "edited"
) %>%
    mutate(
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude)
    )
data %>% glimpse()


# Place Polygons ----
projects <- tribble(
    ~name, ~url, ~place,
    "Graz", "city-nature-challenge-2021-graz", "graz",
    "Wien", "city-nature-challenge-2021-wien", "wien",
    "St. Pölten", "city-nature-challenge-2021-st-poelten", "stpoelten",
    "Hardegg-Thayatal-Podyji", "city-nature-challenge-2021-hardegg-thayatal-podyji", "thayatal",
    "Neusiedler See / Seewinkel", "city-nature-challenge-2021-neusiedler-see-seewinkel", "neusiedlersee",
    "Linz", "city-nature-challenge-2021-linz-linz-land-urfahr-umgebung", "linz",
    "Klagenfurt", "city-nature-challenge-2021-klagenfurt", "klagenfurt",
    "Krems & Wachau", "city-nature-challenge-2021-krems-wachau", "krems",
    "Waidhofen/Ybbs", "city-nature-challenge-2021-waidhofen-ybbs", "waidhofen",
    "Mondsee - Irrsee", "city-nature-challenge-2021-mondsee-irrsee", "mondsee"
)

projects <- projects %>%
    left_join(
        data %>%
            group_by(place) %>%
            summarise(
                observer = length(unique(user_id))
            ),
        by = c("place")
    ) %>%
    mutate(
        label = glue("{name} ({observer})")
    )


polys <- list()
for (i in seq_len(nrow(projects))) {
    url <- glue("https://api.inaturalist.org/v1/projects?q={projects[i,]$url}")
    print(url)
    fetch_url <- fromJSON(url)
    id <- fetch_url$results$project_observation_rules[[1]] %>% pull(operand_id)
    url <- glue("https://api.inaturalist.org/v1/places/{paste(id, collapse = ',')}")
    print(url)
    fetch_url <- fromJSON(url)
    poly <- geojsonsf::geojson_sf(toJSON(fetch_url$results$geometry_geojson), expand_geometries = TRUE)
    poly$name <- projects[i, ]$name
    polys[[i]] <- poly
}
polys <- bind_rows(polys)
polys <- polys %>% left_join(projects, by = "name")

# combined <- polys %>%
#    group_by(name) %>%
#    summarise(
#        geometry = st_combine(geometry)
#    )

p <- ggplot2::ggplot() +
    geom_sf(
        data = austria_states_simplify,
        aes(group = BL),
        inherit.aes = FALSE,
        alpha = 0,
        color = "black",
        size = 0.7
    ) +
    geom_sf(
        data = polys,
        aes(fill = label),
        size = 0
    ) +
    scale_fill_manual(values = colorBlindBlack8) +
    coord_sf() +
    labs(x = "", y = "", fill = "Stadt/Gebiet (Beobachter)") +
    theme_void() +
    theme()
fSaveImages(p, "Places_Map")

# Observations -----
data <- data %>%
    left_join(projects, by = "place")

p <- data %>%
    add_count(name, name = "total") %>%
    count(name, quality_grade, total) %>%
    glimpse() %>%
    mutate(
        name = forcats::fct_reorder(name, total, .desc = TRUE)
    ) %>%
    ggplot(aes(x = name, y = n, fill = quality_grade)) +
    geom_col() +
    geom_text(
        aes(y = total, label = total),
        nudge_y = 400,
        check_overlap = TRUE
    ) +
    scale_fill_manual(
        values = colorBlindBlack8
    ) +
    scale_y_continuous(
        breaks = scales::pretty_breaks(),
        labels = scales::label_number_si()
    ) +
    labs(
        y = "Beobachtungen",
        x = "",
        fill = "Bestimmung"
    ) +
    theme(
        panel.grid.major.y = element_line(),
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = c(0.80, 0.7)
    )

fSaveImages(p, "Observations")

# Graz Dataset ----

graz <- data %>%
    filter(place == "graz") %>%
    drop_na(iconic_taxon_name)



p <- graz %>%
    drop_na(taxon_phylum_name) %>%
    add_count(place, name = "total") %>%
    count(total, taxon_phylum_name) %>%
    mutate(
        p = n / first(total),
        taxon_phylum_name = forcats::fct_reorder(taxon_phylum_name, p, .desc = TRUE)
    ) %>%
    ggplot(aes(x = taxon_phylum_name, y = p, label = n)) +
    geom_col() +
    geom_text(nudge_y = 0.02, check_overlap = TRUE) +
    scale_y_continuous(
        breaks = scales::pretty_breaks(),
        label = scales::label_percent()
    ) +
    labs(
        y = "Beobachtungen Phyla [%]",
        x = ""
    ) +
    theme(
        panel.grid.major.y = element_line(),
        axis.text.x = element_text(angle = 20, hjust = 1),
    )

fSaveImages(p, "Graz_Phylum")


## Insekten -----
p <- graz %>%
    filter(taxon_class_name == "Insecta") %>%
    drop_na(taxon_order_name) %>%
    add_count(taxon_class_name, name = "total") %>%
    count(total, taxon_order_name) %>%
    mutate(
        p = n / total,
        taxon_order_name = forcats::fct_reorder(taxon_order_name, p, .desc = TRUE)
    ) %>%
    ggplot(aes(y = taxon_order_name, x = p, label = n)) +
    geom_col() +
    geom_text(nudge_x = 0.01, check_overlap = TRUE) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(),
        labels = scales::label_percent()
    ) +
    labs(
        y = "Insecta - Ordnung", x = "Beobachtungen [%]"
    ) +
    theme(
        panel.grid.major.x = element_line()
    )

fSaveImages(p, "Graz_Insect_Order", h = 6)