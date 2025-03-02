library(tidyverse)
library(googlesheets4)
library(renv)
library(reactable)

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

bird_dates <- birds |> 
  distinct(taxonomic_order, common_name, safe_date_possible, safe_date_probable)

month_cols <- birds |> 
  select(-c(1:5)) |> 
  names()

breeding_calendar <- birds |> 
  select(-contains("safe")) |> 
  mutate(across(month_cols, replace_na, ""))

glimpse(breeding_calendar)

breeding_calendar |> 
  distinct(`Jan 1`)

breeding_calendar |>   
  reactable(
  columns = list(
    `Jan 1` = colDef(
      style = function(value) {
        if (value == "B") {
          color <- "#c6e0b4"
        } else if (value == "E") {
          color <- "#ffe699"
        } else if (value == "N") {
          color <- "#bdd7ee"
        } else if (value == "M") {
          color <- "#fdbbd4"
        } else {
          color <- "white"
        }
        list(background = color,
          fontWeight = "bold",
        alpha = .5)
        }
    )
  )
  )

breeding_formatting <- function(x) {
  colDef(
    style = function(value) {
      if (value == "B") {
        color <- "#c6e0b4"
      } else if (value == "E") {
        color <- "#ffe699"
      } else if (value == "N") {
        color <- "#bdd7ee"
      } else if (value == "M") {
        color <- "#fdbbd4"
      } else {
        color <- "white"
      }
      list(background = color,
        fontWeight = "bold",
        alpha = .5)
      }
    )
}

breeding_col_styles <- map(month_cols, breeding_formatting)

names(breeding_col_styles) <- month_cols

other_cols <- list(common_name = colDef("Common Name",
filterable = TRUE),
priority_species = colDef("Priority",
filterable = TRUE))

breeding_table_formatting <- c(other_cols, breeding_col_styles)

breeding_calendar |>   
  reactable(columns = breeding_table_formatting)