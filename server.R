library(shiny)
library(bslib)
library(tidyverse)
library(reactable)
library(shinyWidgets)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)
library(santoku)
library(mapgl)

####breeding bird calendar
breeding_calendar_raw <- read_csv("input/breeding_calendar.csv")

breeding_calendar_long <- breeding_calendar_raw |>
  mutate(across(everything(), \(x) replace_na(x, ""))) |>
  select(-taxonomic_order) |>
  pivot_longer(
    cols = -c(common_name, priority_species),
    names_to = "week",
    values_to = "breeding_calendar_code"
  ) |>
  mutate(year_week_index = row_number(), .by = common_name)

year_week_index_df <- breeding_calendar_long |>
  distinct(year_week_index, week)

breeding_calendar_long <- breeding_calendar_long |>
  select(-year_week_index)

breeding_calendar_wide <- breeding_calendar_long |>
  pivot_wider(names_from = week, values_from = breeding_calendar_code)

breeding_color_formatting <- function(x) {
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
      list(background = color, alpha = .5)
    }
  )
}

glossary_table <- tibble(
  Code = c("B", "E", "M", "N"),
  Description = c(
    "Breeding season only",
    "Either migration or breeding",
    "Migration",
    "Non-breeding season"
  )
)

#formatting for glossary
glossary_cols <- names(glossary_table)

glossary_col_styles <- map(glossary_cols, breeding_color_formatting)

names(glossary_col_styles) <- glossary_cols

current_date <- Sys.Date()

current_year <- year(current_date)

format_date <- function(x) {
  str_c(current_year, "-", x) |> ydm()
}

safe_dates <- read_csv("input/bird_safe_dates.csv") |>
  select(
    common_name,
    safe_date_probable_start,
    safe_date_probable_end,
    safe_date_possible_start,
    safe_date_possible_end
  ) |>
  mutate(across(
    c(
      safe_date_probable_start,
      safe_date_probable_end,
      safe_date_possible_start,
      safe_date_possible_end
    ),
    format_date
  )) |>
  rename_with(.fn = ~ str_remove(.x, "safe_"), .cols = contains("date")) |>
  arrange(date_probable_start)

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
    atlas_block,
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

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  current_month <- month(Sys.Date(), label = TRUE, abbr = TRUE) |>
    as.character()

  breeding_calendar <- eventReactive(
    c(input$toggle_current_month, input$toggle_exclude_na_code),
    {
      #filter with input$toggle_current_month
      x <- if (input$toggle_current_month) {
        current_month_index <- year_week_index_df |>
          filter(str_detect(week, current_month)) |>
          slice_head(n = 1) |>
          pull(year_week_index)

        breeding_calendar_long |>
          left_join(year_week_index_df) |>
          filter(year_week_index >= current_month_index) |>
          select(-year_week_index)
      } else if (input$toggle_current_month == FALSE) {
        breeding_calendar_long
      }

      #pivot wider at the end
      x <- x |>
        pivot_wider(names_from = week, values_from = breeding_calendar_code)

      #filter with input$toggle_exclude_na_code
      x <- if (input$toggle_exclude_na_code == TRUE) {
        first_col <- x |>
          select(3) |>
          names() |>
          sym()

        x |>
          filter(!!first_col != "")
      } else if (input$toggle_exclude_na_code == FALSE) {
        x
      }
    }
  )

  breeding_table_formatting <- reactive({
    #formatting for calendar
    month_cols <- breeding_calendar() |>
      select(-c(1:2)) |>
      names()

    breeding_col_styles <- map(month_cols, breeding_color_formatting)

    names(breeding_col_styles) <- month_cols

    other_cols <- list(
      common_name = colDef(
        "Common Name",
        filterable = TRUE,
        sticky = "left",
        style = list(borderRight = "2px solid #eee"),
        headerStyle = list(borderRight = "1px solid #eee")
      ),
      priority_species = colDef("Priority", filterable = TRUE)
    )

    c(other_cols, breeding_col_styles)
  })

  output$calendar <- renderReactable({
    breeding_calendar() |>
      reactable(columns = breeding_table_formatting())
  })

  output$dates_table <- renderReactable({
    reactable(
      safe_dates,
      columns = list(
        common_name = colDef(
          "Common Name",
          filterable = TRUE,
          sticky = "left",
          style = list(borderRight = "2px solid #eee"),
          headerStyle = list(borderRight = "1px solid #eee")
        ),
        date_probable_start = colDef(
          name = "Probable start",
          cell = function(value) strftime(value, "%b %e")
        ),
        date_probable_end = colDef(
          name = "Probable end",
          cell = function(value) strftime(value, "%b %e")
        ),
        date_possible_start = colDef(
          name = "Possible start",
          cell = function(value) strftime(value, "%b %e")
        ),
        date_possible_end = colDef(
          name = "Possible end",
          cell = function(value) strftime(value, "%b %e")
        )
      )
    )
  })

  output$glossary_table <- renderReactable({
    glossary_table |>
      reactable(columns = glossary_col_styles)
  })

  #block effort map
  output$block_effort_map <- renderMaplibre({
    block_summary <- block_summary |>
      filter(season == input$season_variable)

    maplibre_view(
      block_summary,
      column = input$block_variable,
      legend_positon = "top-right"
    )
  })

  output$block_completion_table <- renderReactable({
    block_summary |>
      st_drop_geometry() |>
      filter(season == input$season_variable_table) |>
      reactable(
        filterable = TRUE,
        resizable = TRUE,
        columns = list(
          atlas_block = colDef(
            name = "PBA3 Atlas Block",
            minWidth = 150,
            cell = function(value) {
              url <- paste0("https://ebird.org/atlaspa/block/", value)
              tags$a(href = url, target = "_blank", value)
            }
          ),
          season = colDef(name = "Season", minWidth = 100),
          checklist_count = colDef(name = "Checklists", minWidth = 100),
          species_observed = colDef(name = "Total species", minWidth = 120),
          birders = colDef(name = "Atlasers", minWidth = 100),
          duration_hours = colDef(name = "Effort hours", minWidth = 150),
          effort_distance_km = colDef(
            name = "Effort distance (km)",
            minWidth = 175
          )
        )
      )
  })
}
