#clean up Excel table, save to /inputs
library(tidyverse)

birds <- read_csv("inputs/pa_breeding_bird_calendar_2025_02.csv")

birds

month_cols <- paste(rep(month.abb, each = 4), rep(1:4, 12))

df_names <- c("taxonomic_order", "common_name", "safe_date_probable_start", "safe_date_possible_start", "safe_date_possible_end", "safe_date_probable_end")

df_names <- c(df_names, month_cols)

colnames(birds) <- df_names

birds <- birds |> 
  mutate(priority_species = str_detect(common_name, "\\*"),
common_name = str_remove(common_name, "\\*")) |> 
  select(taxonomic_order, common_name, priority_species, everything())

glimpse(birds)

bird_safe_dates <- birds |> 
  select(taxonomic_order, common_name, contains("safe")) |> 
  distinct()

breeding_calendar <- birds |> 
  select(-contains("safe")) |> 
  mutate(across(all_of(month_cols), \(x) replace_na(x, "")))

write_csv(breeding_calendar, "inputs/breeding_calendar.csv")

write_csv(bird_safe_dates, "inputs/bird_safe_dates.csv")