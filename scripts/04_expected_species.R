library(tidyverse)
library(auk)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)
library(mapgl)
library(ebbr)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

#seasons
seasons <- tibble(
  season = c(rep(c("All seasons"), 12), rep("Breeding", 5), rep("Winter", 3)),
  month = c(
    month.abb,
    c("Apr", "May", "Jun", "Jul", "Aug"),
    c("Dec", "Jan", "Feb")
  )
)

breeding_months <- seasons |>
  filter(season == "Breeding") |>
  pull(month)

#block summary
####block effort
block_summary <- read_parquet(
  "input/block_summary_seasons.parquet",
  as_data_frame = FALSE
) |>
  mutate(
    duration_hours = round(duration_hours, 2),
    effort_distance_km = round(effort_distance_km, 2)
  ) |>
  select(
    pba3_block,
    season,
    species_observed,
    Observed,
    Possible,
    Probable,
    Confirmed,
    checklist_count,
    birders,
    duration_hours,
    effort_distance_km,
    geometry
  ) |>
  st_as_sf()

#checklists
tic()
output_file <- "input/pa_breeding_bird_atlas_processed.txt"

ebd_df <- read_delim(output_file, delim = "\t") |>
  mutate(across(breeding_code, str_squish)) |>
  mutate(breeding_category = coalesce(breeding_category, "C1")) |>
  mutate(
    observation_month = month(observation_date, label = TRUE, abbr = TRUE),
    observation_count = case_when(
      observation_count == "X" ~ "1",
      .default = observation_count
    ),
  ) |>
  mutate(observation_count = parse_number(observation_count)) |>
  rename(pba3_block = atlas_block)
toc()

glimpse(ebd_df)

ebd_df |>
  distinct(observation_month)

ebd_df <- ebd_df |>
  semi_join(
    filter(seasons, season == "Breeding"),
    by = join_by(observation_month == month)
  )

calc_species_observed_pct <- function(x, y) {
  x |>
    filter(pba3_block == y) |>
    select(
      observation_month,
      pba3_block,
      checklist_id,
      common_name,
      observation_count
    ) |>
    complete(
      pba3_block,
      observation_month = breeding_months,
      checklist_id,
      common_name
    ) |>
    mutate(
      observation_month = factor(observation_month, levels = month.abb)
    ) |>
    summarize(
      observations = sum(!is.na(observation_count)),
      checklist_count = n_distinct(checklist_id),
      observed_pct = mean(!is.na(observation_count)),
      .by = c(pba3_block, common_name, observation_month)
    ) |>
    arrange(desc(observed_pct))
}

calc_species_observed_pct(ebd_df, "40080D1SE")

#calculated species observation % per block
expected_species <- ebd_df |>
  group_nest(pba3_block, keep = TRUE) |>
  mutate(
    species_obs_pct_data = map2(
      data,
      pba3_block,
      ~ calc_species_observed_pct(.x, .y)
    )
  ) |>
  select(-c(pba3_block, data)) |>
  unnest(species_obs_pct_data)

expected_species |>
  ggplot(aes(checklist_count, observed_pct)) +
  geom_bin_2d() +
  scale_fill_viridis_c()

expected_species |>
  ggplot(aes(observed_pct, color = observation_month)) +
  geom_density() +
  geom_rug() +
  facet_wrap(vars(observation_month), scales = "free")

expected_species |>
  filter(common_name == "Northern House Wren") |>
  ggplot(aes(observed_pct)) +
  geom_histogram()

expected_species |>
  filter(common_name == "Northern House Wren") |>
  ggplot(aes(observation_month, observed_pct, group = pba3_block)) +
  geom_line()

expected_species_pa <- expected_species |>
  summarize(
    checklist_count = sum(checklist_count),
    observations = sum(observations),
    .by = common_name
  )

prior <- ebb_fit_prior(expected_species, observations, checklist_count)

prior

obs_fitted <- augment(prior, data = expected_species)

obs_fitted |>
  slice_sample(prop = .1, weight_by = .fitted) |>
  ggplot(aes(.raw, .fitted, color = checklist_count)) +
  geom_point(size = .2) +
  geom_abline(color = "red") +
  scale_color_viridis_c(trans = "log10") +
  geom_hline(yintercept = tidy(prior)$mean, color = "red", lty = 2) +
  labs(x = "Raw observation %", y = "Shrunken observation %")

obs_fitted |>
  filter(common_name %in% c("Ovenbird", "Chipping Sparrow")) |>
  select(common_name, .raw, .fitted) |>
  pivot_longer(-common_name) |>
  ggplot(aes(value, color = common_name, lty = name)) +
  geom_density()

expected_species_pa |>
  add_ebb_estimate(
    observations,
    checklist_count
  ) |>
  filter(common_name == "Northern House Wren") |>
  ggplot(aes(.fitted)) +
  geom_histogram()


block_summary |>
  filter(season == "Breeding") |>
  select(pba3_block) |>
  left_join(expected_species) |>
  filter(common_name == "Northern House Wren", observation_month == "Jun") |>
  mutate(observed_pct_weighted = observed_pct * checklist_count) |>
  maplibre_view(column = "observed_pct_weighted")
