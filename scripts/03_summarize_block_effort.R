library(tidyverse)
library(auk)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)

tic()
output_file <- "input/pa_breeding_bird_atlas_processed.txt"

ebd_df <- read_delim(output_file, delim = "\t") |>
  mutate(across(breeding_code, str_squish))
toc()

# tic()
# ebd_df_rds <- read_rds("input/pa_breeding_bird_atlas_processed.rds") |>
#   st_as_sf(coords = c("longitude", "latitude"))
# toc()

glimpse(ebd_df)

ebd_df |>
  count(breeding_category, breeding_code) |>
  arrange(breeding_category)

ebd_df |>
  distinct(checklist_id)

st_read("input/PABBA_2nd/PABBA_2nd.shp") |>
  glimpse()

atlas_blocks <- st_read("input/PABBA_2nd/PABBA_2nd.shp") |>
  select(NAME, NAME2, BLOCK_ID, REGION, ISPRIORITY) |>
  rename(BLOCK_ID_old = BLOCK_ID, BLOCK_ID = NAME)

glimpse(atlas_blocks)

checklist_coords <- ebd_df |>
  distinct(checklist_id, longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE)

st_crs(checklist_coords) <- st_crs(atlas_blocks)

block_checklist_geo <- st_join(atlas_blocks, checklist_coords)

block_checklist_geo

block_summary <- left_join(ebd_df, block_checklist_geo) |>
  group_by(BLOCK_ID) |>
  summarize(
    checklist_count = n_distinct(checklist_id),
    species_observed = n_distinct(common_name),
    birders = n_distinct(observer_id),
    duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
    effort_distance_km = sum(effort_distance_km, na.rm = TRUE)
  ) |>
  ungroup()

confirmed_species <- left_join(ebd_df, block_checklist_geo) |>
  filter(breeding_category == "C4") |>
  distinct(BLOCK_ID, common_name) |>
  count(BLOCK_ID, name = "confirmed_species")

confirmed_species

df_list <- list(atlas_blocks, block_summary, confirmed_species)

block_summary <- reduce(df_list, left_join, by = "BLOCK_ID")

attr(block_summary, "sf_column")

write_parquet(block_summary, "input/block_summary.parquet")

block_summary <- read_parquet(
  "input/block_summary.parquet",
  as_data_frame = FALSE
) |>
  st_as_sf()

block_summary |>
  ggplot() +
  geom_sf(aes(fill = confirmed_species)) +
  scale_fill_viridis_c()
