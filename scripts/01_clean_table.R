#clean up Excel table, save to /inputs
library(tidyverse)

birds <- read_csv("inputs/PA-Bird-Atlas-Breeding-Guidelines-Start-and-End-Dates-SORTED-1.xlsx - Sheet1.csv",
col_names = FALSE,
skip = 2)

birds

month_cols <- paste(rep(month.abb, each = 4), rep(1:4, 12))

df_names <- c("taxonomic_order", "common_name", "safe_date_probable", "safe_date_possible")

df_names <- c(df_names, month_cols)

colnames(birds) <- df_names

birds <- birds |> 
  mutate(priority_species = str_detect(common_name, "\\*")) |> 
  select(taxonomic_order, common_name, priority_species, everything())

glimpse(birds)

bird_safe_dates <- birds |> 
  distinct(taxonomic_order, common_name, safe_date_possible, safe_date_probable)

month_cols <- birds |> 
  select(-c(1:5)) |> 
  names()

breeding_calendar <- birds |> 
  select(-contains("safe")) |> 
  mutate(across(all_of(month_cols), replace_na, ""))

write_csv(breeding_calendar, "inputs/breeding_calendar.csv")

write_csv(bird_safe_dates, "inputs/bird_safe_dates.csv")
