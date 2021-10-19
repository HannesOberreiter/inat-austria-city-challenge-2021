# Add Identifier too our dataset from API -----
library(here)
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

# Import Data ---------
data <- readxl::read_xlsx(
  "data/16-10-2021-raw.xlsx",
  col_types = "text",
  sheet = "edited"
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
    ident_list[[ids[[j]]]] <- tibble(
      id = ids[[j]],
      obs_id = paste(ident[[j]]$user$id, collapse = ","),
      obs_name = paste(ident[[j]]$user$login, collapse = ",")
    )
  }
  i <- ix
}

res <- bind_rows(ident_list) # %>%
# mutate(id = as.integer(id)) %>%
# arrange(id) %>%
# glimpse()
res %>% write_excel_csv2(file = glue("{here()}/data/identifier.csv"))