library(readxl)
library(tidyverse)

read_excel("data/PBA3 Focal Species_updated.xlsx", col_names = "common_name") |>
  mutate(is_focal = TRUE) |>
  write_csv("data/focal_species.csv")
