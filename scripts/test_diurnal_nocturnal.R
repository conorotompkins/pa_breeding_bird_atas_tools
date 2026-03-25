library(tidyverse)
library(auk)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)
library(mapgl)
library(hms)

source("functions/mode.R")

options(scipen = 999, digits = 4)

theme_set(theme_bw())

breeding_lookup <- tibble(
  breeding_category = c("0", "C1", "C2", "C3", "C4"), #consider C0 instead of 0 to be consistent
  breeding_category_desc = c(
    "Not Observed",
    "Observed",
    "Possible",
    "Probable",
    "Confirmed"
  ),
  breeding_rank = c(0:4)
)

location_sunrise_sunset <- read_parquet(
  "input/location_sunrise_sunset.parquet"
)

glimpse(location_sunrise_sunset)

location_sunrise_sunset |>
  count(latitude, longitude, observation_datetime) |>
  filter(n > 1) |>
  nrow() ==
  0

block_name_lookup <- read_csv("input/block_name_lookup.csv") |>
  distinct(block_id, region, block_name) |>
  rename(pba3_block = block_id, block_region = region)

tic()
ebd_df <- read_parquet("input/pa_breeding_bird_atlas_processed.parquet")
toc()

glimpse(ebd_df)

ebd_df |>
  filter(is.na(observation_datetime)) |>
  count(protocol_name)

ebd_df |>
  distinct(checklist_id, observation_datetime) |>
  count(checklist_id) |>
  count(n)

dupe_start_times <- ebd_df |>
  distinct(checklist_id, observation_datetime) |>
  count(checklist_id) |>
  filter(n > 1)

dupe_start_times

ob_dt_fixed <- ebd_df |>
  distinct(
    pba3_block,
    checklist_id,
    observer_id,
    observation_datetime
  ) |>
  semi_join(dupe_start_times) |>
  separate_longer_delim(observer_id, delim = ",") |>
  mutate(
    observation_datetime_fixed = mode(observation_datetime),
    .by = checklist_id
  ) |>
  distinct(checklist_id, observation_datetime_fixed)

ob_dt_fixed

test1 <- ebd_df |>
  left_join(block_name_lookup) |>
  distinct(
    pba3_block,
    block_name,
    checklist_id,
    observer_id,
    observation_datetime,
    longitude,
    latitude,
    duration_minutes
  ) |>
  left_join(ob_dt_fixed) |>
  mutate(
    observation_datetime = case_when(
      !is.na(observation_datetime_fixed) ~ observation_datetime_fixed,
      .default = observation_datetime
    )
  ) |>
  select(-c(observation_datetime_fixed, observer_id)) |>
  distinct()

test1 |>
  distinct(checklist_id, observation_datetime) |>
  count(checklist_id) |>
  count(n) |>
  filter(n > 1) |>
  nrow() ==
  0

test2 <- test1 |>
  left_join(
    location_sunrise_sunset,
    by = join_by(
      longitude,
      latitude,
      observation_datetime
    )
  ) |>
  mutate(
    flag_is_diurnal_checklist = between(
      observation_datetime,
      sunrise - minutes(40),
      sunset + minutes(20)
    ),
    checklist_type = case_when(
      flag_is_diurnal_checklist == TRUE ~ "diurnal",
      flag_is_diurnal_checklist == FALSE ~ "nocturnal",
      is.na(flag_is_diurnal_checklist) ~ "unknown"
    )
  ) |>
  select(-flag_is_diurnal_checklist)

test2 |>
  count(checklist_type)

test3 <- test2 |>
  summarize(
    duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
    .by = c(pba3_block, block_name, checklist_type)
  ) |>
  pivot_wider(
    names_from = checklist_type,
    values_from = duration_hours
  ) |>
  select(pba3_block, block_name, diurnal, nocturnal, unknown)

test3


test_emsworth <- test2 |>
  filter(block_name == "Pittsburgh East CE")

test_emsworth

test_emsworth |>
  count(checklist_type)

test_emsworth_flag <- test2 |>
  group_by(checklist_type) |>
  slice_sample(n = 1000) |>
  ungroup() |>
  mutate(id = row_number(), observation_date = as_date(observation_datetime)) |>
  mutate(across(c(sunrise, sunset, observation_datetime), as_hms)) |>
  arrange(observation_datetime)

test_emsworth_flag |>
  ggplot(aes(x = sunrise, xend = sunset, y = observation_date)) +
  geom_segment(lwd = .2) +
  geom_point(aes(
    x = observation_datetime,
    color = checklist_type
  )) +
  labs(x = "time window", color = "is_diurnal")

#export
test_emsworth |>
  mutate(
    observation_date = as_date(observation_datetime),
    observation_time = as_hms(observation_datetime),
    sunrise_date = as_date(sunrise),
    sunrise_time = as_hms(sunrise),
    sunset_date = as_date(sunset),
    sunset_time = as_hms(sunset)
  ) |>
  select(-c(observation_datetime, sunrise, sunset)) |>
  mutate(across(everything(), as.character)) |>
  write_csv("~/Downloads/Emsworth_NE_export.csv")
