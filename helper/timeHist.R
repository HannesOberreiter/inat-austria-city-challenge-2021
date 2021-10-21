# Libs ----
library(tidyverse)

library(here)
library(glue)

library(scales)
library(ggtext)
library(cowplot)
library(ggforce)

library(jsonlite)
library(lubridate)

# Constants ----
places <- c("Österreich" = 8057, "Steiermark" = 143475)
ids <- c("Fungi" = 47170, "Plantae" = 47126, "Animalia" = 1)
data <- list()

# Load Data ----
if (file.exists("data/data.rds")) {
    data <- readRDS("data/data.rds")
} else {
    for (i in seq_along(places)) {
        data[[i]] <- list()
        place_id <- places[[i]]
        place_name <- names(places)[[i]]
        for (j in seq_along(ids)) {
            taxon_id <- ids[[j]]
            taxon_name <- names(ids)[[j]]

            obs_url <- glue(
                "https://api.inaturalist.org/v1/observations/histogram?taxon_id={taxon_id}&place_id={place_id}&d1=2008&date_field=observed&interval=month"
            )
            obs_df <- fromJSON(obs_url)$result$month %>%
                bind_rows() %>%
                pivot_longer(everything(), names_to = "time", values_to = "obs") %>%
                mutate(
                    place = place_name,
                    taxon = taxon_name
                )
            data[[i]][[j]] <- obs_df
        }
    }
    data <- bind_rows(data)
    saveRDS(data, file = "data/data.rds")
}

# TidyData ----
cbp1 <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

color_df <- tibble(
    taxon = c("Fungi", "Animalia", "Plantae"),
    color = c("#E69F00", "#D55E00", "#009E73")
)
df <- data %>%
    left_join(color_df) %>%
    mutate(
        time = ymd(time),
        month = month(time),
        quarter = quarter(time),
        year = year(time),
        taxon = taxon %>% fct_relevel("Fungi", after = Inf)
    ) %>%
    group_by(place, taxon) %>%
    mutate(
        csum = cumsum(obs)
    ) %>%
    ungroup() %>%
    glimpse()

df_rect <- df %>%
    group_by(year) %>%
    summarise(
        xmin = min(time),
        xmean = mean(time),
        xmax = max(time),
        ymin = -Inf,
        ymax = Inf
    ) %>%
    mutate(
        alpha = ifelse(year %% 2 == 0, NA, "#56B4E9")
    )

df_total <- df %>%
    group_by(time, place) %>%
    summarise(total = sum(obs)) %>%
    ungroup() %>%
    group_by(place) %>%
    mutate(total = cumsum(total)) %>%
    ungroup() %>%
    glimpse()

df_TextAt <- tribble(
    ~time, ~timeend, ~title, ~text, ~y, ~yend,
    "2009-01-01", "2009-03-01", "2008-12-31", "Mehr als 800 Beobachtungen<br/>für Österreich auf iNaturalist.", 850, 70000,
    "2013-01-01", "2012-06-01", "2012-12-31", "Vier Jahre später waren es bereits <br/>über 5.500 Beobachtungen.", 5500, 70000,
    "2015-01-01", "2015-06-01", "2014-12-31", "Nur mehr zwei Jahre später wurde <br/>die 10.000er Marke überschritten.", 10000, 110000,
    "2020-01-01", "2018-01-01", "2019-12-31", "Über 100.000 eingetragene Beobachtungen!", 103633, 180000,
    "2021-05-01", "2019-05-01", "2021-05-01", "Aktuell sind es über in Summe <br/> über 300.000 Beobachtungen.", 319095, 250000
) %>%
    mutate(
        time = as.Date(time),
        timeend = as.Date(timeend),
        title = as.Date(title) %>% format("%d.%m.%Y")
    )

p <- df %>%
    filter(place == "Österreich") %>%
    ggplot(aes(x = time, y = csum, group = taxon)) +
    geom_rect(
        data = df_rect,
        aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax,
            fill = alpha,
        ),
        alpha = 0.2,
        inherit.aes = FALSE
    ) +
    geom_richtext(
        data = df_rect,
        aes(
            x = xmean,
            y = 340000,
            label = year
        ),
        label.colour = NA,
        fill = NA,
        fontface = "bold",
        size = 5,
        alpha = 0.5,
        inherit.aes = FALSE
    ) +
    geom_area(
        aes(fill = color)
    ) +
    geom_line(
        data = df_total %>% filter(place == "Steiermark" & time > "2012-01-01") %>% mutate(time = if_else(time == last(time), last(time) - 10, time)),
        aes(x = time, y = total, group = "A"),
        size = 2,
        color = "#999999",
        inherit.aes = FALSE,
        lineend = "round"
    ) +
    geom_line(
        data = df_total %>% filter(place == "Österreich"),
        aes(x = time, y = total, group = "A"),
        color = "black",
        inherit.aes = FALSE,
        lineend = "round"
    ) +
    annotate(
        geom = "curve",
        x = df_TextAt$timeend,
        xend = df_TextAt$time,
        y = df_TextAt$yend,
        yend = df_TextAt$y + 1000,
        curvature = -.3,
        arrow = arrow(length = unit(5, "mm"))
    ) +
    geom_richtext(
        data = df_TextAt,
        aes(
            x = timeend,
            y = yend,
            label = glue::glue("<b>{title}</b>:<br/> {text}"),
            nudge_y = c(1, 1, 1, 1, 0),
            hjust = c(0, 0, 0, 1, 1),
            vjust = 0.9
        ),
        inherit.aes = FALSE,
        size = 5
    ) +
    geom_richtext(
        data = df %>%
            filter(place == "Österreich" & time == last(time)),
        aes(time, csum,
            group = taxon,
            label = glue::glue(
                "<b style='font-size:20pt;'>{taxon}</b><br>{format(csum, big.mark  = '.', decimal.mark = ',', width = 7, justify = 'right')}"
            ),
            color = color,
            nudge_x = 1,
            hjust = 0,
            vjust = 0.9
        ),
        inherit.aes = FALSE,
        position = "stack",
        size = 5,
        label.colour = NA,
        fill = NA
    ) +
    coord_cartesian(clip = "off") +
    scale_x_date(
        date_breaks = "1 month",
        date_labels = " ",
        expand = c(0, 0),
    ) +
    geom_richtext(
        data = df_total %>% filter(place == "Steiermark" & time == last(time)),
        aes(
            time,
            total,
            group = 1,
            label = glue::glue("<b style='font-size:15pt;'>Steiermark</b><br>{format(total, big.mark  = '.', decimal.mark = ',', trim = FALSE)}", .trim = FALSE),
            color = "#999999",
            nudge_x = 1,
            hjust = 0,
            vjust = 0.9
        ),
        inherit.aes = FALSE,
        position = "stack",
        size = 5,
        label.colour = NA,
        fill = NA
    ) +
    scale_y_continuous(
        breaks = scales::breaks_pretty(3),
        labels = label_number(
            accuracy = NULL, scale = 1,
            prefix = "", suffix = "",
            big.mark = ".", decimal.mark = ","
        ),
        limits = c(0, 3.5 * 10^5),
        expand = c(0, 0)
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha(range = c(.1, 1)) +
    labs(
        title = "iNaturalist - Österreich",
        subtitle = "Eingetragene Beobachtungen",
        caption = "(c) Hannes Oberreiter  ||  API iNaturalist.org - Stand: 15.05.2021",
        y = "", x = ""
    ) +
    theme_minimal_hgrid() +
    theme(
        panel.grid.major.y = element_line(color = "black"),
        panel.background = element_rect(fill = "#56b3e969"),
        axis.text.x = element_text(color = "grey60", margin = margin(t = 4)),
        axis.ticks.x = element_line(color = "grey60"),
        axis.ticks.length.x = unit(.4, "lines"),
        axis.line.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(35, 100, 5, 5),
        plot.caption = element_text(margin = margin(t = -10)),
        axis.text.y = element_text(size = 15, hjust = 0, vjust = -0.5, color = "#000000", margin = margin(r = -60)),
        axis.ticks.y = element_blank()
    )


ggsave("output/histInat.pdf", p, width = 18, height = 9.5)
ggsave("output/histInat.png", p, width = 18, height = 9.5)