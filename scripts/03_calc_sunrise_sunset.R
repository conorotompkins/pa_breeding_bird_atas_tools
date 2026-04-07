library(tidyverse)
library(mirai)
library(sf)
library(suntools)
library(hms)
library(tictoc)
library(arrow)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

daemons(n = 10)

#checklists
tic()
ebd_df <- read_parquet("data/pa_breeding_bird_atlas_processed.parquet")
toc()

glimpse(ebd_df)

attr(ebd_df$observation_datetime, "tzone")

checklist_time_loc <- ebd_df |>
  arrange(observation_datetime) |>
  distinct(
    observation_datetime,
    longitude,
    latitude
  )

glimpse(checklist_time_loc)

checklist_time_loc |>
  count(latitude, longitude, observation_datetime) |>
  filter(n > 1) |>
  nrow() ==
  0

my_sunriset <- function(x, direction) {
  #calculate sunrise and sunset for one observation
  coords <- matrix(
    c(x$longitude, x$latitude),
    nrow = 1
  )

  sunrise <- suntools::sunriset(
    coords,
    x$observation_datetime,
    direction = "sunrise",
    POSIXct.out = TRUE
  )

  sunset <- suntools::sunriset(
    coords,
    x$observation_datetime,
    direction = "sunset",
    POSIXct.out = TRUE
  )

  tibble::tibble(sunrise = sunrise$time, sunset = sunset$time)
}

my_sunriset(slice_head(checklist_time_loc, n = 1))

calc_sunrise_sunset <- function(x) {
  #perform my_sunriset on dataframes with map
  x |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::group_nest(id) |>
    dplyr::mutate(test = purrr::map(data, my_sunriset)) |>
    dplyr::select(test) |>
    tidyr::unnest(c(test))
}

slice_head(checklist_time_loc, n = 2) |>
  calc_sunrise_sunset()

tic()
## 1596.743 sec elapsed
Sys.time()
location_sunrise_sunset <- checklist_time_loc |>
  mutate(
    id = row_number(),
    chunk = id %/% 1000
  ) |>
  group_nest(chunk) |>
  mutate(
    sunrise_sunset = map(
      .x = data,
      .f = in_parallel(
        \(x) calc_sunrise_sunset(x),
        calc_sunrise_sunset = calc_sunrise_sunset, # pass main function
        my_sunriset = my_sunriset # pass helper it depends on
      )
    )
  )
Sys.time()
toc()

glimpse(location_sunrise_sunset)

location_sunrise_sunset <- location_sunrise_sunset |>
  select(data, sunrise_sunset) |>
  unnest(c(data, sunrise_sunset)) |>
  select(-id)

glimpse(location_sunrise_sunset)

location_sunrise_sunset |>
  count(longitude, latitude, observation_datetime) |>
  filter(n > 1) |>
  nrow() ==
  0

attr(location_sunrise_sunset$observation_datetime, "tzone")
attr(location_sunrise_sunset$sunrise, "tzone")
attr(location_sunrise_sunset$sunset, "tzone")

expected_tz <- attr(location_sunrise_sunset$observation_datetime, "tzone")

write_parquet(location_sunrise_sunset, "data/location_sunrise_sunset.parquet")

location_sunrise_sunset <- read_parquet("data/location_sunrise_sunset.parquet")

attr(location_sunrise_sunset$observation_datetime, "tzone") == expected_tz
attr(location_sunrise_sunset$sunrise, "tzone") == expected_tz
attr(location_sunrise_sunset$sunset, "tzone") == expected_tz
