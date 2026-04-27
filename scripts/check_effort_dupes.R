library(tidyverse)
library(auk)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)
library(mapgl)
library(glue)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

ebd_df <- open_dataset("data/pa_breeding_bird_atlas_processed.parquet")

ebd_df |>
  distinct(pba3_block, checklist_id, duration_minutes, effort_distance_km) |>
  count(pba3_block) |>
  collect() |>
  ggplot(aes(n)) +
  geom_histogram()

ebd_df |>
  distinct(
    pba3_block,
    checklist_id,
    observation_datetime,
    duration_minutes,
    effort_distance_km
  ) |>
  head() |>
  collect() |>
  glimpse()


ebd_df |>
  distinct(
    pba3_block,
    checklist_id,
    observation_datetime,
    duration_minutes,
    effort_distance_km
  ) |>
  map(class)

ebd_df |>
  distinct(
    pba3_block,
    checklist_id,
    observation_datetime,
    duration_minutes,
    effort_distance_km
  ) |>
  add_count(checklist_id) |>
  filter(n > 1) |>
  arrange(desc(n), checklist_id) |>
  head() |>
  collect() |>
  dput() |>
  clipr::write_clip()

mode_vec <- function(x) {
  # Drop NAs but remember if everything was NA
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0L) {
    # All NA: return a single NA of the same type
    return(x[NA_integer_][1])
  }

  # Work with the original vector so we preserve attributes (e.g. tz)
  ux <- unique(x_no_na)

  # Tabulate using indices into `ux`
  tab <- tabulate(match(x_no_na, ux))

  # Pick first mode in case of ties
  ux[which.max(tab)]
}

# ebd_df |>
#   distinct(
#     pba3_block,
#     checklist_id,
#     observation_datetime,
#     duration_minutes,
#     effort_distance_km
#   ) |>
#   summarize(
#     observation_datetime_mode = mode_vec(observation_datetime),
#     duration_minutes_mode = mode_vec(duration_minutes),
#     effort_distance_km_mode = mode_vec(effort_distance_km),
#     .by = checklist_id
#   )

block_effort_naive <- ebd_df |>
  distinct(pba3_block, checklist_id, duration_minutes, effort_distance_km) |> #need to check if different observer IDs can have different effort in the same checklist
  summarize(
    duration_hours_total_naive = sum(duration_minutes, na.rm = TRUE) / 60,
    effort_distance_km_naive = sum(effort_distance_km, na.rm = TRUE),
    .by = pba3_block
  )

block_effort_adj <- ebd_df |>
  distinct(
    pba3_block,
    checklist_id,
    observation_datetime,
    duration_minutes,
    effort_distance_km
  ) |>
  #take the min of observation_datetime, max of duration_minutes and effort_distance_km
  summarize(
    observation_datetime = min(observation_datetime, na.rm = TRUE),
    duration_minutes = max(duration_minutes, na.rm = TRUE),
    effort_distance_km = max(effort_distance_km, na.rm = TRUE),
    .by = c(pba3_block, checklist_id)
  )

block_effort_adj |>
  add_count(checklist_id) |>
  filter(n > 1) |>
  collect()

block_effort_adj |>
  filter(pba3_block == "G12168212") |>
  collect()

block_effort_adj <- block_effort_adj |>
  summarize(
    observation_datetime = min(observation_datetime, na.rm = TRUE),
    duration_hours_total_adj = sum(duration_minutes, na.rm = TRUE) / 60,
    effort_distance_km_adj = sum(effort_distance_km, na.rm = TRUE),
    .by = pba3_block
  )

df_compare <- left_join(block_effort_naive, block_effort_adj) |>
  mutate(
    duration_hours_diff = duration_hours_total_adj - duration_hours_total_naive,
    effort_distance_diff = effort_distance_km_adj - effort_distance_km_naive
  )

df_compare |>
  summarize(mean(duration_hours_diff), mean(effort_distance_diff)) |>
  collect()

df_compare |>
  collect() |>
  ggplot(aes(duration_hours_diff)) +
  geom_histogram()

df_compare |>
  filter(pba3_block == "40075C4CE") |>
  collect() |>
  glimpse()

df_compare |>
  filter(abs(duration_hours_diff) > 0 | abs(effort_distance_diff) > 0) |>
  collect() |>
  view()
