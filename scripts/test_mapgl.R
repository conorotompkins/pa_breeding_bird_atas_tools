library(shiny)
library(bslib)
library(tidyverse)
library(reactable)
library(shinyWidgets)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)
library(mapgl)

source("R/map_checklist_count.R")

seasons <- open_dataset("data/seasons.parquet") |>
  collect()

ebd_df <- open_dataset("data/pa_breeding_bird_atlas_processed.parquet")

schema(ebd_df)

season_choices <- c("All seasons", "Breeding", "Winter")

block_choices <- c("39077G6SW", "40080D1SE")

seasons_filtered <- seasons |>
  filter(season == season_choices[1])

block_checklist_count <- ebd_df |>
  filter(pba3_block == block_choices[2]) |>
  collect() |>
  semi_join(seasons_filtered, by = c("observation_month" = "month")) |>
  distinct(checklist_id, longitude, latitude) |>
  count(longitude, latitude, name = "checklist_count") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

block_checklist_count

#block_checklist_count |>
# ggplot(aes(checklist_count)) +
# geom_histogram()

maplibre_view(block_checklist_count, column = "checklist_count")

block_checklist_map <- block_checklist_count

block_checklist_map

block_checklist_map |>
  st_drop_geometry() |>
  count(checklist_count)

exp_seq <- function(start, end, length) {
  if (start <= 0 || end <= 0) {
    stop("start and end must be > 0")
  }
  if (length < 1 || length != as.integer(length)) {
    stop("length must be a positive integer")
  }

  x <- exp(seq(log(start), log(end), length.out = length))

  x <- round(x, 0)
}

#manual mapping
count_range <- exp_seq(
  min(block_checklist_map$checklist_count),
  max(block_checklist_map$checklist_count),
  length = 10
) |>
  unique()

#circle_sizes <- seq(2, 10, length.out = length(count_range))

circle_sizes <- exp_seq(
  3,
  15,
  length(count_range)
)

print(circle_sizes)

maplibre() |>
  fit_bounds(block_checklist_map, animate = FALSE) |>
  add_circle_layer(
    id = "count-circles",
    source = block_checklist_map,
    circle_radius = step_expr(
      column = "checklist_count",
      base = 2,
      values = count_range,
      stops = circle_sizes
    ),
    circle_color = "#1f78b4",
    circle_opacity = 0.8,
    circle_stroke_color = "#ffffff",
    circle_stroke_width = 1,
    tooltip = "checklist_count"
  ) |>
  add_legend(
    legend_title = "Checklists",
    values = count_range,
    colors = rep("#1f78b4", length(circle_sizes)),
    type = "categorical",
    sizes = circle_sizes,
    position = "top-right"
  )


#function
map_checklist_count(block_checklist_count)
