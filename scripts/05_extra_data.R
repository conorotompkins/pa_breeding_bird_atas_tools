library(readxl)
library(tidyverse)
library(arrow)

read_excel("data/PBA3 Focal Species_updated.xlsx", col_names = "common_name") |>
  mutate(is_focal = TRUE) |>
  write_csv("data/focal_species.csv")

focal_species <- read_csv("data/focal_species.csv")

ebd_df <- open_dataset("data/pa_breeding_bird_atlas_processed.parquet") |>
  distinct(common_name) |>
  collect()

anti_join(focal_species, ebd_df)

anti_join(ebd_df, focal_species)
