library(here)
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

# Import Data ---------
clean_names <- c(
  "id", "location", "district", "state", "country",
  "latitude", "longitude", "accuracy", "observed_date",
  "family", "species", "url", "comment", "quality", "downloaded_date", "observer"
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

i <- 1
ix <- 0
testoffset <- 0
ident_list <- list()
while (i < nrow(data) - testoffset) {
  print(paste("start", i))
  ix <- ifelse((i + 200) < nrow(data) - testoffset, ix + 200, nrow(data) - testoffset)
  print(paste("end", ix))

  # fetching
  id <- data$id[i:ix]
  url <- glue("https://api.inaturalist.org/v1/observations?id={paste(id, collapse =',')}&order=desc&order_by=created_at")
  fetch_url <- fromJSON(url)
  result <- bind_rows(fetch_url$results)
  result <- bind_rows(fetch_url$results)
  ids <- as.character(result$id)
  ident <- fetch_url$results$identifications
  for (j in 1:length(ids)) {
    ident_list[[ids[[j]]]] <- tibble(id = ids[[j]], obs_id = paste(ident[[j]]$user$id, collapse = ","), obs_name = paste(ident[[j]]$user$login, collapse = ","))
  }
  i <- ix
}

res <- bind_rows(ident_list) %>%
  mutate(id = as.integer(id)) %>%
  arrange(id) %>%
  glimpse()
res %>% write_excel_csv2(file = glue("{here()}/raw/identifier.csv"))