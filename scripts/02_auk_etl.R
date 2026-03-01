library(tidyverse)
library(auk)
library(tictoc)
library(sf)
library(tools)

auk_file <- "input/ebd_US-PA_202401_202601_smp_relJan-2026/ebd_US-PA_202401_202601_smp_relJan-2026.txt"

file.exists(auk_file) == TRUE

release_pattern <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\\d{4}.txt"

ebird_release <- str_extract(auk_file, release_pattern) |>
  file_path_sans_ext()

ebird_release

write_file(ebird_release, "input/ebird_release.txt")

output_file <- "input/pa_breeding_bird_atlas_data_raw.txt"

tic()
ebd <- auk_file |>
  auk_ebd() |>
  auk_date(date = c("2024-01-01", "2026-01-31")) |>
  auk_project("Pennsylvania Bird Atlas") |>
  auk_filter(file = output_file, overwrite = TRUE) |>
  # 4. read text file into r data frame
  read_ebd()
toc()
#944.459 sec elapsed

glimpse(ebd)

write_delim(ebd, "input/pa_breeding_bird_atlas_processed.txt", delim = "\t")

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
