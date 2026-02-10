library(tidyverse)
library(auk)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)
library(mapgl)

#checklists
tic()
output_file <- "input/pa_breeding_bird_atlas_processed.txt"

ebd_df <- read_delim(output_file, delim = "\t") |>
  mutate(across(breeding_code, str_squish)) |>
  mutate(breeding_category = coalesce(breeding_category, "C1")) |>
  mutate(observation_month = month(observation_date, label = TRUE, abbr = TRUE))
toc()

glimpse(ebd_df)

ebd_df |>
  count(breeding_category, breeding_code) |>
  arrange(breeding_category)

breeding_lookup <- tibble(
  breeding_category = c("C1", "C2", "C3", "C4"),
  breeding_category_desc = c("Observed", "Possible", "Probable", "Confirmed"),
  breeding_rank = c(1:4)
)

breeding_lookup

ebd_df <- left_join(ebd_df, breeding_lookup, by = "breeding_category")

glimpse(ebd_df)

ebd_df |>
  filter(is.na(atlas_block)) |>
  distinct(checklist_id, observation_date, longitude, latitude) |>
  mutate(observation_date = as.character(observation_date)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "NAD83") |>
  maplibre_view()

#block map
#BLOCK_ID 42080145 is duplicated
st_read("input/PABBA_2nd/PABBA_2nd.shp") |>
  glimpse()

atlas_blocks <- st_read("input/PABBA_2nd/PABBA_2nd.shp") |>
  select(NAME) |>
  rename(BLOCK_ID = NAME) |>
  group_by(BLOCK_ID) |>
  summarize()

glimpse(atlas_blocks)

checklist_block <- ebd_df |>
  distinct(atlas_block, checklist_id, longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"))

st_crs(checklist_block) <- st_crs(atlas_blocks)

checklist_block |>
  st_drop_geometry() |>
  as_tibble() |>
  count(atlas_block) |>
  ggplot(aes(n)) +
  geom_histogram() +
  geom_vline(xintercept = 400)

atlas_3_centroids <- checklist_block |>
  #filter(atlas_block == "40080D1SE") |>
  drop_na(atlas_block) |>
  group_by(atlas_block) |>
  slice_sample(n = 400) |>
  summarize() |>
  st_convex_hull() |>
  st_point_on_surface()

atlas_3_centroids

atlas_3_centroids |>
  ggplot() +
  geom_sf()

tic()
block_checklist_geo <- st_join(
  atlas_blocks,
  atlas_3_centroids,
  join = st_covers,
  largest = FALSE
)
toc()

glimpse(block_checklist_geo)

block_checklist_geo |>
  st_drop_geometry() |>
  as_tibble() |>
  distinct(atlas_block, BLOCK_ID) |>
  count(atlas_block, sort = TRUE)

block_checklist_geo |>
  st_drop_geometry() |>
  as_tibble() |>
  distinct(atlas_block, BLOCK_ID) |>
  count(BLOCK_ID, sort = TRUE)

atlas_block_mismatch <- block_checklist_geo |>
  filter(BLOCK_ID == "42080145") |>
  distinct(BLOCK_ID, atlas_block)

atlas_block_mismatch

maplibre(bounds = atlas_blocks) |>
  add_fill_layer(
    source = atlas_blocks,
    id = "blocks",
    fill_opacity = .2,
    tooltip = "BLOCK_ID"
  ) |>
  add_symbol_layer(
    source = atlas_blocks,
    id = "block_labels",
    text_field = get_column("BLOCK_ID")
  )

atlas_blocks |>
  st_drop_geometry() |>
  count(BLOCK_ID) |>
  filter(n > 1)


ggplot() +
  geom_sf(
    data = semi_join(atlas_blocks, atlas_block_mismatch)
  ) +
  geom_sf(
    data = semi_join(checklist_block, atlas_block_mismatch)
  )

maplibre(bounds = semi_join(atlas_blocks, atlas_block_mismatch)) |>
  add_fill_layer(
    source = semi_join(atlas_blocks, atlas_block_mismatch),
    id = "blocks",
    fill_opacity = .2,
    tooltip = "BLOCK_ID"
  ) |>
  add_symbol_layer(
    source = semi_join(atlas_blocks, atlas_block_mismatch),
    id = "block_labels",
    text_field = get_column("BLOCK_ID")
  ) |>
  add_circle_layer(
    id = "coords",
    source = semi_join(checklist_block, atlas_block_mismatch),
    tooltip = c("atlas_block")
  )

#map checklist coords from a single atlas_block on top of a BLOCK_ID square

#seasons
seasons <- tibble(
  season = c(rep(c("All seasons"), 12), rep("Breeding", 5), rep("Winter", 3)),
  month = c(
    month.abb,
    c("Apr", "May", "Jun", "July", "Aug"),
    c("Dec", "Jan", "Feb")
  )
)

seasons

glimpse(ebd_df)

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

  # block_summary <- checklist_df |>
  #   group_by(atlas_block) |>
  #   summarize(
  #     checklist_count = n_distinct(checklist_id),
  #     species_observed = n_distinct(common_name),
  #     birders = n_distinct(observer_id),
  #     duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
  #     effort_distance_km = sum(effort_distance_km, na.rm = TRUE)
  #   ) |>
  #   ungroup()

  block_checklist_count <- checklist_df |>
    distinct(atlas_block, checklist_id) |>
    summarize(checklist_count = n_distinct(checklist_id), .by = atlas_block)

  block_species_observed <- checklist_df |>
    select(atlas_block, common_name) |>
    distinct() |>
    summarize(species_observed = n_distinct(common_name), .by = atlas_block)

  block_birders <- checklist_df |>
    distinct(atlas_block, observer_id) |>
    summarize(birders = n_distinct(observer_id), .by = atlas_block)

  block_effort <- checklist_df |>
    distinct(atlas_block, checklist_id, duration_minutes, effort_distance_km) |>
    summarize(
      duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
      effort_distance_km = sum(effort_distance_km, na.rm = TRUE),
      .by = atlas_block
    )

  block_species_coded <- checklist_df |>
    distinct(atlas_block, common_name, breeding_category_desc, breeding_rank) |>
    group_by(atlas_block, common_name) |>
    filter(breeding_rank == max(breeding_rank)) |>
    ungroup() |>
    count(atlas_block, breeding_category_desc, breeding_rank) |>
    select(-breeding_rank) |>
    pivot_wider(names_from = breeding_category_desc, values_from = n)

  df_list <- list(
    block_checklist_count,
    block_species_observed,
    block_birders,
    block_effort,
    block_species_coded
  )

  block_summary <- reduce(df_list, left_join, by = "atlas_block")

  block_summary
}

x <- summarize_season(
  checklist_df = ebd_df,
  block_df = block_checklist_geo,
  season_filter = "All seasons"
)

x |>
  filter(atlas_block == "40080D1SE") |>
  view()

season_summaries <- map(
  set_names(c("All seasons", "Breeding", "Winter")),
  ~ summarize_season(
    checklist_df = ebd_df,
    block_df = block_checklist_geo,
    season_filter = .x
  )
)

season_summaries |> str(max.level = 1)

block_summary_seasons <- list_rbind(season_summaries, names_to = "season")

block_summary_seasons <- left_join(
  block_checklist_geo,
  block_summary_seasons,
  by = "atlas_block"
)

block_summary_seasons |>
  slice_head(n = 1)

block_summary_seasons |>
  filter(season == "All seasons") |>
  maplibre_view(column = "Confirmed")

glimpse(block_summary_seasons)

block_summary_seasons |>
  ggplot() +
  geom_sf(aes(fill = Confirmed)) +
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
  geom_sf(aes(fill = Confirmed)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(season), ncol = 1) +
  theme_bw()
