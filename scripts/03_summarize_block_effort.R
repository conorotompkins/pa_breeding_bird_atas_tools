library(tidyverse)
library(auk)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)

tic()
output_file <- "input/pa_breeding_bird_atlas_processed.txt"

ebd_df <- read_delim(output_file, delim = "\t") |>
  mutate(across(breeding_code, str_squish)) |>
  mutate(observation_month = month(observation_date, label = TRUE, abbr = TRUE))
toc()

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

seasons <- tibble(
  season = c(rep(c("All seasons"), 12), rep("Breeding", 5), rep("Winter", 3)),
  month = c(
    month.abb,
    c("Apr", "May", "Jun", "July", "Aug"),
    c("Dec", "Jan", "Feb")
  )
)

seasons

summarize_season <- function(
  checklist_df,
  block_df,
  seasons_df = seasons,
  season_filter,
  atlas_blocks_data = atlas_blocks
) {
  seasons_df <- seasons_df |>
    filter(season == season_filter)

  checklist_df <- checklist_df |>
    semi_join(
      seasons_df,
      by = join_by(observation_month == month)
    )

  block_summary <- left_join(checklist_df, block_df) |>
    group_by(BLOCK_ID) |>
    summarize(
      checklist_count = n_distinct(checklist_id),
      species_observed = n_distinct(common_name),
      birders = n_distinct(observer_id),
      duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
      effort_distance_km = sum(effort_distance_km, na.rm = TRUE)
    ) |>
    ungroup()

  confirmed_species <- left_join(checklist_df, block_df) |>
    filter(breeding_category == "C4") |>
    distinct(BLOCK_ID, common_name) |>
    count(BLOCK_ID, name = "confirmed_species")

  confirmed_species

  df_list <- list(atlas_blocks_data, block_summary, confirmed_species)

  block_summary <- reduce(df_list, left_join, by = "BLOCK_ID")

  block_summary
}

x <- summarize_season(
  checklist_df = ebd_df,
  block_df = block_checklist_geo,
  season_filter = "All seasons"
)

season_summaries <- map(
  set_names(c("All seasons", "Breeding", "Winter")),
  ~ summarize_season(
    checklist_df = ebd_df,
    block_df = block_checklist_geo,
    season_filter = .x
  )
)

season_summaries |> str(max.level = 1)

block_summary_seasons <- list_rbind(season_summaries, names_to = "season") |>
  st_as_sf()

glimpse(block_summary_seasons)

block_summary_seasons |>
  ggplot() +
  geom_sf(aes(fill = confirmed_species)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(season), ncol = 1) +
  theme_bw()

write_parquet(block_summary_seasons, "input/block_summary_seasons.parquet")

block_summary_seasons <- read_parquet(
  "input/block_summary_seasons.parquet",
  as_data_frame = FALSE
) |>
  st_as_sf()

block_summary_seasons |>
  ggplot() +
  geom_sf(aes(fill = confirmed_species)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(season), ncol = 1) +
  theme_bw()
