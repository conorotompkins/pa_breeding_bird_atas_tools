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

source("R/mode.R")

#block name lookup file
block_name_lookup <- read_csv("data/block_name_lookup.csv") |>
  distinct(block_id, region, block_name) |>
  rename(pba3_block = block_id, block_region = region)

#location sunrise/sunset
location_sunrise_sunset <- read_parquet(
  "data/location_sunrise_sunset.parquet"
)

glimpse(location_sunrise_sunset)

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

breeding_lookup

#checklists
tic()
ebd_df <- read_parquet("data/pa_breeding_bird_atlas_processed.parquet")
toc()

glimpse(ebd_df)

print("Block lookup IDs from block_name_lookup that are NOT in ebd_df")
block_name_lookup |>
  anti_join(ebd_df |> distinct(pba3_block))

print("Block IDs from ebd_df that are NOT in block_name_lookup")
ebd_df |>
  distinct(pba3_block) |>
  anti_join(block_name_lookup)

ebd_df |>
  summarize(min(observation_date), max(observation_date))

ebd_df |>
  count(breeding_category, breeding_code) |>
  arrange(breeding_category)

ebd_df |>
  distinct(breeding_category)

ebd_df <- ebd_df |>
  left_join(breeding_lookup, by = join_by(breeding_category))

glimpse(ebd_df)

ebd_df |>
  distinct(breeding_category, breeding_code, breeding_rank) |>
  arrange(breeding_rank)

print("Checking for checklists with >1 observation_datetime")
ebd_df |>
  distinct(checklist_id, observation_datetime) |>
  count(checklist_id) |>
  filter(n > 1)

ebd_df |>
  distinct(checklist_id, observation_datetime) |>
  filter(checklist_id == "G11700387")

# ebd_df |>
#   filter(checklist_id == "G11700387") |>
#   view()

#shared checklists can have >1 start time??
ebd_df |>
  select(checklist_id, observation_datetime, observer_id) |>
  distinct() |>
  count(checklist_id) |>
  count(n)

dupe_start_times <- ebd_df |>
  select(checklist_id, observation_datetime) |>
  distinct() |>
  count(checklist_id) |>
  filter(n > 1)

dupe_start_times

dupe_start_times_check <- dupe_start_times |>
  nrow() >
  1

glue(
  "Non-distinct checklist start times present: ",
  dupe_start_times_check
)

glue(nrow(dupe_start_times), " checklists have >1 start times")

ebd_df |>
  distinct(
    pba3_block,
    checklist_id,
    observer_id,
    observation_date,
    time_observations_started,
    observation_datetime
  ) |>
  semi_join(dupe_start_times) |>
  mutate(across(everything(), as.character)) |>
  write_csv("~/Downloads/dupe_start_times.csv")

#find modal start time per checklist. I will use that for all observers for each checklist.
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


# ebd_df |>
#   filter(is.na(pba3_block)) |>
#   distinct(checklist_id, observation_date, longitude, latitude) |>
#   mutate(observation_date = as.character(observation_date)) |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = "NAD83") |>
#   maplibre_view()

#block map
st_read("data/PABBA_2nd/PABBA_2nd.shp") |>
  glimpse()

pba2_blocks <- st_read("data/PABBA_2nd/PABBA_2nd.shp") |>
  select(BLOCK_ID) |>
  rename(pba2_block = BLOCK_ID)

print("Checkiing for duplicate pba2_block")
pba2_check <- pba2_blocks |>
  st_drop_geometry() |>
  count(pba2_block) |>
  filter(n > 1) |>
  nrow() ==
  0

glue("pba2_block is unique: ", pba2_check)

# maplibre(bounds = pba2_blocks) |>
#   add_fill_layer(
#     source = pba2_blocks,
#     id = "blocks",
#     fill_opacity = .2,
#     tooltip = "pba2_block"
#   ) |>
#   add_symbol_layer(
#     source = pba2_blocks,
#     id = "block_labels",
#     text_field = get_column("pba2_block")
#   )

#distinct of checklist coordinates and pba3_block
checklist_pba3_block <- ebd_df |>
  distinct(pba3_block, checklist_id, longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"))

st_crs(checklist_pba3_block) <- st_crs(pba2_blocks)

checklist_pba3_block |>
  st_drop_geometry() |>
  as_tibble() |>
  count(pba3_block) |>
  ggplot(aes(n)) +
  geom_histogram() +
  geom_vline(xintercept = 400)

#calculate centroid of all checklist coordinates in each pba3_block
pba3_centroids <- checklist_pba3_block |>
  drop_na(pba3_block) |>
  group_by(pba3_block) |>
  slice_sample(n = 1000) |>
  summarize() |>
  st_convex_hull() |>
  st_point_on_surface()

pba3_centroids |>
  ggplot() +
  geom_sf(size = .5)

#join pba2 blocks with pba3 centroids
tic()
block_checklist_geo <- st_join(
  pba2_blocks,
  pba3_centroids,
  join = st_covers,
  largest = FALSE
)
toc()

block_checklist_geo <- block_checklist_geo |>
  mutate(
    pba2_block = case_when(
      pba3_block == "40075F2SE" ~ 4932,
      .default = pba2_block
    ),
  ) |>
  filter(!(pba2_block == 4932 & is.na(pba3_block)))

glimpse(block_checklist_geo)

block_checklist_geo |>
  st_drop_geometry() |>
  as_tibble() |>
  distinct(pba3_block, pba2_block) |>
  count(pba3_block, sort = TRUE)

print("Checking that pba3_block and pba2_block are 1-1 (excluding NAs)")
block_relationship_na_check <- block_checklist_geo |>
  st_drop_geometry() |>
  as_tibble() |>
  distinct(pba3_block, pba2_block) |>
  drop_na(pba3_block) |>
  count(pba3_block, sort = TRUE) |>
  filter(n > 1) |>
  nrow() ==
  0

glue("pba3_block and pba2_block are 1-1: ", block_relationship_na_check)

print("Checking that pba3_block and pba2_block are 1-1 (excluding NAs)")
block_relationship_check <- block_checklist_geo |>
  st_drop_geometry() |>
  as_tibble() |>
  distinct(pba3_block, pba2_block) |>
  count(pba2_block, sort = TRUE) |>
  filter(n > 1) |>
  nrow() ==
  0

glue("pba3_block and pba2_block are 1-1: ", block_relationship_check)

block_checklist_geo |>
  filter(pba2_block == 4932) |>
  st_drop_geometry() |>
  as_tibble()

pba3_block_mismatch <- block_checklist_geo |>
  filter(pba2_block == 3670) |>
  distinct(pba2_block, pba3_block)

pba3_block_mismatch

checklist_pba3_block |>
  filter(pba3_block == "40075F2SE")

maplibre(
  bounds = checklist_pba3_block |>
    filter(pba3_block == "40075F2SE")
) |>
  add_fill_layer(
    id = "pba2_blocks",
    source = pba2_blocks,
    fill_color = "blue",
    fill_opacity = .2,
    tooltip = "pba2_block"
  ) |>
  add_circle_layer(
    id = "checklists",
    source = checklist_pba3_block |>
      filter(pba3_block == "40075F2SE")
  )

block_checklist_geo |>
  filter(pba3_block == "40075F2SE")

maplibre(
  bounds = checklist_pba3_block |>
    filter(pba3_block == "40075F2SE")
) |>
  add_fill_layer(
    id = "pba2_blocks",
    source = block_checklist_geo,
    fill_color = "blue",
    fill_opacity = .2,
    tooltip = "pba2_block"
  ) |>
  add_symbol_layer(
    id = "pba2_block_id",
    source = block_checklist_geo,
    text_field = get_column("pba2_block")
  ) |>
  add_circle_layer(
    id = "pba3_centroids",
    source = pba3_centroids,
    circle_radius = 6,
    circle_color = "red",
    tooltip = "pba3_block"
  ) |>
  add_circle_layer(
    id = "checklists",
    source = checklist_pba3_block |>
      filter(pba3_block == "40075F2SE"),
    circle_radius = 4,
    circle_color = "yellow"
  )


# ggplot() +
#   geom_sf(
#     data = semi_join(pba2_blocks, pba3_block_mismatch, by = "pba2_block")
#   ) +
#   geom_sf(
#     data = semi_join(
#       checklist_block,
#       pba3_block_mismatch,
#       by = "pba3_block"
#     )
#   )

# maplibre(bounds = semi_join(pba2_blocks, pba3_block_mismatch)) |>
#   add_fill_layer(
#     source = semi_join(pba2_blocks, pba3_block_mismatch),
#     id = "blocks",
#     fill_opacity = .2,
#     tooltip = "pba2_block"
#   ) |>
#   add_symbol_layer(
#     source = semi_join(pba2_blocks, pba3_block_mismatch),
#     id = "block_labels",
#     text_field = get_column("pba2_block")
#   ) |>
#   add_circle_layer(
#     id = "coords",
#     source = semi_join(
#       pba3_centroids,
#       pba3_block_mismatch,
#       by = "pba3_block"
#     ),
#     tooltip = c("pba3_block")
#   )

#map checklist coords from a single pba3_block on top of a BLOCK_ID square

#seasons
seasons <- tibble(
  season = c(rep(c("All seasons"), 12), rep("Breeding", 5), rep("Winter", 3)),
  month = c(
    month.abb,
    c("Apr", "May", "Jun", "Jul", "Aug"),
    c("Dec", "Jan", "Feb")
  )
)

seasons

write_parquet(seasons, "data/seasons.parquet")

glimpse(ebd_df)

#compare block breeding rank between PBA2 and PBA3
#expected species based on PBA2
pbba2_df <- read_csv("data/PBBA2_block_species_codes.csv") |>
  rename(
    block = 1,
    pba3_block = 2,
    block_name = 3
  ) |>
  select(1:220) |>
  pivot_longer(
    -c(1:3),
    names_to = "common_name",
    values_to = "breeding_category_desc"
  ) |>
  filter(!str_detect(common_name, "N/A")) |>
  mutate(
    breeding_category_desc = case_when(
      breeding_category_desc == "Observed/Possible" ~ "Observed",
      .default = breeding_category_desc
    )
  )

pba2_breeding_rank_max <- pbba2_df |>
  left_join(breeding_lookup, by = join_by(breeding_category_desc)) |>
  distinct(pba3_block, common_name, breeding_category_desc, breeding_rank) |>
  rename(
    pba2_breeding_category_max = breeding_category_desc,
    pba2_breeding_rank_max = breeding_rank
  )

pba3_breeding_rank_max <- ebd_df |>
  group_by(pba3_block, common_name) |>
  filter(breeding_rank == max(breeding_rank)) |>
  ungroup() |>
  distinct(pba3_block, common_name, breeding_category_desc, breeding_rank) |>
  rename(
    pba3_breeding_category_max = breeding_category_desc,
    pba3_breeding_rank_max = breeding_rank
  )

pba2_confirmed_blocks <- distinct(pba2_breeding_rank_max, pba3_block)

pba3_confirmed_blocks <- distinct(pba3_breeding_rank_max, pba3_block)

print("Checking that all blocks in pba3_confirmed are in pba2_confirmed")
confirmed_blocks_check1 <- pba3_confirmed_blocks |>
  anti_join(pba2_confirmed_blocks) |>
  nrow() ==
  0

glue(
  "All blocks in pba3_confirmed are in pba2_confirmed: ",
  confirmed_blocks_check1
)

print("Checking that all blocks in pba2_confirmed should be in pba3_confirmed")
confirmed_blocks_check2 <- pba2_confirmed_blocks |>
  anti_join(pba3_confirmed_blocks) |>
  nrow() ==
  0

glue(
  "All blocks in pba2_confirmed are in pba3_confirmed: ",
  confirmed_blocks_check2
)

#only compare blocks that exist in PBA2 and PBA3
atlas_max_breeding_rank_comparison <- bind_rows(
  pba2_breeding_rank_max |> distinct(pba3_block, common_name),
  pba3_breeding_rank_max |> distinct(pba3_block, common_name)
) |>
  distinct() |>
  inner_join(pba2_breeding_rank_max, by = join_by(pba3_block, common_name)) |>
  inner_join(pba3_breeding_rank_max, by = join_by(pba3_block, common_name)) |>
  replace_na(list(
    pba2_breeding_category_max = "Not Observed",
    pba2_breeding_rank_max = 0,
    pba3_breeding_category_max = "Not Observed",
    pba3_breeding_rank_max = 0
  )) |>
  left_join(block_name_lookup, by = join_by(pba3_block)) |>
  mutate(
    block_name = coalesce(
      block_name,
      "Unknown block name"
    ),
    block_region = coalesce(
      block_region,
      "Unknown region"
    )
  ) |>
  select(pba3_block, block_name, block_region, everything())

glimpse(atlas_max_breeding_rank_comparison)

atlas_max_breeding_rank_comparison |>
  filter(pba3_block == "40080D1SE") |>
  filter(pba3_breeding_rank_max < pba2_breeding_rank_max) |>
  arrange(pba3_block, desc(pba2_breeding_rank_max))

atlas_max_breeding_rank_comparison |>
  arrange(pba3_block, desc(pba2_breeding_rank_max)) |>
  write_parquet("data/atlas_max_breeding_category_comparison.parquet")

atlas_max_breeding_rank_comparison |>
  filter(pba2_breeding_rank_max > pba3_breeding_rank_max) |>
  arrange(pba3_block, desc(pba2_breeding_rank_max)) |>
  write_parquet("data/missing_pba2_breeding_category_obs.parquet")

confirmed_pba2_unconfirmed_pba3 <- atlas_max_breeding_rank_comparison |>
  summarize(
    species_count = n(),
    pct_missing_pba2_confirmations = mean(
      pba2_breeding_rank_max > pba3_breeding_rank_max
    ),
    .by = pba3_block
  ) |>
  arrange(desc(species_count))

confirmed_pba2_unconfirmed_pba3 |>
  ggplot(aes(species_count, pct_missing_pba2_confirmations)) +
  geom_bin_2d() +
  scale_fill_viridis_c()

confirmed_pba2_unconfirmed_pba3 |>
  write_parquet("data/confirmed_pba2_unconfirmed_pba3.parquet")

#summary metrics by season
summarize_season <- function(
  checklist_df,
  block_df,
  seasons_df = seasons,
  season_filter,
  pba2_block_data = pba2_blocks
) {
  seasons_df <- seasons_df |>
    filter(season == season_filter)

  checklist_df <- checklist_df |>
    semi_join(
      seasons_df,
      by = join_by(observation_month == month)
    )

  # block_summary <- checklist_df |>
  #   group_by(pba3_block) |>
  #   summarize(
  #     checklist_count = n_distinct(checklist_id),
  #     species_observed = n_distinct(common_name),
  #     birders = n_distinct(observer_id),
  #     duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
  #     effort_distance_km = sum(effort_distance_km, na.rm = TRUE)
  #   ) |>
  #   ungroup()

  block_checklist_count <- checklist_df |>
    distinct(pba3_block, checklist_id) |>
    summarize(checklist_count = n_distinct(checklist_id), .by = pba3_block)

  block_species_observed <- checklist_df |>
    select(pba3_block, common_name) |>
    distinct() |>
    summarize(species_observed = n_distinct(common_name), .by = pba3_block)

  block_birders <- checklist_df |>
    distinct(pba3_block, observer_id) |>
    separate_rows(observer_id, sep = ",") |>
    summarize(birders = n_distinct(observer_id), .by = pba3_block)

  block_effort <- checklist_df |>
    distinct(pba3_block, checklist_id, duration_minutes, effort_distance_km) |> #need to check if different observer IDs can have different effort in the same checklist
    summarize(
      duration_hours_total = sum(duration_minutes, na.rm = TRUE) / 60,
      effort_distance_km = sum(effort_distance_km, na.rm = TRUE),
      .by = pba3_block
    )

  block_species_coded <- checklist_df |>
    distinct(pba3_block, common_name, breeding_category_desc, breeding_rank) |>
    group_by(pba3_block, common_name) |>
    filter(breeding_rank == max(breeding_rank)) |>
    ungroup() |>
    count(pba3_block, breeding_category_desc, breeding_rank) |>
    select(-breeding_rank) |>
    pivot_wider(names_from = breeding_category_desc, values_from = n)

  block_dn_raw <- checklist_df |>
    distinct(
      pba3_block,
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
        !is.na(observation_datetime_fixed) ~ observation_datetime_fixed, #replace inconsistent start times with modal start time for the checklist
        .default = observation_datetime
      )
    ) |>
    select(-c(observation_datetime_fixed, observer_id)) |>
    distinct() |>
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

  block_dn_summary <- block_dn_raw |>
    summarize(
      duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
      .by = c(pba3_block, checklist_type)
    ) |>
    pivot_wider(
      names_from = checklist_type,
      values_from = duration_hours,
      names_prefix = "duration_hours_"
    ) |>
    select(
      pba3_block,
      duration_hours_diurnal,
      duration_hours_nocturnal,
      duration_hours_unknown
    )

  block_dn_date <- block_dn_raw |>
    mutate(
      observation_date = as_date(observation_datetime)
    ) |>
    summarize(
      duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
      .by = c(pba3_block, checklist_type, observation_date)
    ) |>
    pivot_wider(
      names_from = checklist_type,
      values_from = duration_hours,
      names_prefix = "duration_hours_"
    ) |>
    select(
      pba3_block,
      observation_date,
      duration_hours_diurnal,
      duration_hours_nocturnal,
      duration_hours_unknown
    ) |>
    group_nest(pba3_block, .key = "effort_breakdown")

  block_dn_summary <- block_dn_summary |>
    left_join(block_dn_date)

  df_list <- list(
    block_checklist_count,
    block_species_observed,
    block_birders,
    block_effort,
    block_species_coded,
    block_dn_summary
  )

  block_summary <- reduce(df_list, left_join, by = "pba3_block")

  block_summary
}

x <- summarize_season(
  checklist_df = ebd_df,
  block_df = block_checklist_geo,
  season_filter = "All seasons"
)

x |>
  filter(pba3_block == "40080D1SE") |>
  glimpse()

x |>
  filter(pba3_block == "40080D1SE") |>
  select(effort_breakdown) |>
  unnest(effort_breakdown) |>
  mutate(
    observation_month = month(observation_date, label = TRUE, abbr = TRUE)
  ) |>
  summarize(
    across(starts_with("duration"), \(x) sum(x, na.rm = TRUE)),
    .by = observation_month
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

block_summary_seasons <- list_rbind(season_summaries, names_to = "season")

block_checklist_geo |>
  st_drop_geometry() |>
  count(pba3_block) |>
  as_tibble() |>
  filter(n > 1)

#need to set join argument to deal with many-to-many
block_summary_seasons <- left_join(
  block_checklist_geo,
  block_summary_seasons,
  by = "pba3_block"
)

block_summary_seasons <- block_summary_seasons |>
  left_join(
    confirmed_pba2_unconfirmed_pba3 |>
      select(-species_count) |>
      mutate(season = "All seasons")
  ) |>
  left_join(block_name_lookup, by = join_by(pba3_block)) |>
  mutate(
    block_name = coalesce(
      block_name,
      "Unknown block name"
    ),
    block_region = coalesce(
      block_region,
      "Unknown region"
    )
  ) |>
  select(pba3_block, block_name, block_region, everything())

glimpse(block_summary_seasons)

# block_checklist_geo |>
#   filter(row_number() == 2)

# block_summary_seasons |>
#   filter(pba3_block == "42080A1CW")

# block_summary_seasons |>
#   filter(row_number() == 1951)

# block_checklist_geo |>
#   filter(pba3_block == "40076C2NW")

# block_summary_seasons |>
#   slice_head(n = 1)

block_summary_seasons |>
  filter(season == "All seasons") |>
  select(-effort_breakdown) |>
  maplibre_view(column = "duration_hours_diurnal")

block_summary_seasons |>
  ggplot() +
  geom_sf(aes(fill = Confirmed)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(season), ncol = 1) +
  theme_bw()

write_parquet(block_summary_seasons, "data/block_summary_seasons.parquet")

block_summary_seasons_test <- read_parquet(
  "data/block_summary_seasons.parquet",
  as_data_frame = FALSE
) |>
  st_as_sf()

block_summary_seasons_test |>
  ggplot() +
  geom_sf(aes(fill = Confirmed)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(season), ncol = 1) +
  theme_bw()
