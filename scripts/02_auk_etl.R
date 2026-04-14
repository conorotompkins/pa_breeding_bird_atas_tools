library(tidyverse)
library(auk)
library(tictoc)
library(sf)
library(tools)
library(arrow)

auk_file <- "data/ebd_US-PA_202401_202602_smp_relFeb-2026/ebd_US-PA_202401_202602_smp_relFeb-2026.txt"

file.exists(auk_file) == TRUE

release_pattern <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\\d{4}.txt"

ebird_release <- str_extract(auk_file, release_pattern) |>
  file_path_sans_ext()

ebird_release

write_file(ebird_release, "data/ebird_release.txt")

output_file <- "data/pa_breeding_bird_atlas_data_raw.txt"

tic()
ebd <- auk_file |>
  auk_ebd() |>
  auk_date(date = c("2024-01-01", "2026-01-31")) |>
  auk_project("Pennsylvania Bird Atlas") |>
  auk_filter(file = output_file, overwrite = TRUE) |>
  # 4. read text file into r data frame
  read_ebd()
toc()
# 901.116 sec elapsed

glimpse(ebd)

ebd <- ebd |>
  mutate(across(c(breeding_code, breeding_category), str_squish)) |>
  mutate(breeding_category = coalesce(breeding_category, "C1")) |> #should this be C0 instead of C1?
  mutate(
    observation_month = month(observation_date, label = TRUE, abbr = TRUE),
    observation_datetime = str_c(
      observation_date,
      time_observations_started,
      sep = " "
    ) |>
      ymd_hms(tz = "America/New_York")
  ) |>
  rename(pba3_block = atlas_block)

write_parquet(ebd, "data/pa_breeding_bird_atlas_processed.parquet")

ebd |> distinct(project_names)

ebd |>
  st_as_sf(coords = c("longitude", "latitude")) |>
  slice_sample(prop = .1) |>
  ggplot() +
  geom_sf(alpha = .01, size = .5) +
  theme_void()

ebd |>
  distinct(checklist_id) |>
  nrow()
