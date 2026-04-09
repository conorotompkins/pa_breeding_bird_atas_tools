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
library(quarto)
library(gt)
library(basemaps)

options(shiny.fullstacktrace = FALSE)

set_defaults(map_service = "esri", map_type = "natgeo_world_map")

####ebird release
ebird_release <- read_file("data/ebird_release.txt")

####breeding bird calendar
breeding_calendar_raw <- read_csv("data/breeding_calendar.csv")

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

breeding_season_glossary_table <- tibble(
  Season = c("B", "E", "M", "N"),
  Description = c(
    "Breeding season only",
    "Either migration or breeding",
    "Migration",
    "Non-breeding season"
  )
)

#formatting for glossary
season_glossary_cols <- names(breeding_season_glossary_table)

season_glossary_col_styles <- map(
  season_glossary_cols,
  breeding_color_formatting
)

names(season_glossary_col_styles) <- season_glossary_cols

current_date <- Sys.Date()

current_year <- year(current_date)

format_date <- function(x) {
  str_c(current_year, "-", x) |> ydm()
}

safe_dates <- read_csv("data/bird_safe_dates.csv") |>
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
block_summary <- open_dataset(
  "data/block_summary_seasons.parquet"
) |>
  mutate(
    duration_hours_total = round(duration_hours_total, 2),
    effort_distance_km = round(effort_distance_km, 2)
  ) |>
  select(
    pba3_block,
    block_name,
    block_region,
    season,
    species_observed,
    Observed,
    Possible,
    Probable,
    Confirmed,
    checklist_count,
    birders,
    duration_hours_total,
    duration_hours_diurnal,
    duration_hours_nocturnal,
    duration_hours_unknown,
    effort_distance_km,
    pct_missing_pba2_confirmations,
    geometry
  ) |>
  st_as_sf()

ebd_df <- open_dataset("data/pa_breeding_bird_atlas_processed.parquet")

seasons <- open_dataset("data/seasons.parquet") |>
  collect()

#block-atlas comparison
missing_pba2_breeding_category_obs <- read_parquet(
  "data/missing_pba2_breeding_category_obs.parquet"
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  current_month <- month(Sys.Date(), label = TRUE, abbr = TRUE) |>
    as.character()

  breeding_calendar <- eventReactive(
    c(
      input$toggle_current_month,
      input$toggle_exclude_na_code,
      input$toggle_show_priority_column
    ),
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

      x <- if (input$toggle_show_priority_column) {
        x
      } else {
        select(x, -priority_species)
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
      select(-any_of(c("common_name", "priority_species"))) |>
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
      )
    )

    other_cols <- if (input$toggle_show_priority_column) {
      priority_col <- list(
        priority_species = colDef("Priority", filterable = TRUE)
      )

      c(other_cols, priority_col)
    } else {
      other_cols
    }

    c(other_cols, breeding_col_styles)
  })

  output$calendar <- renderReactable({
    breeding_calendar() |>
      reactable(
        columns = breeding_table_formatting(),
        pagination = FALSE
      )
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
      ),
      pagination = FALSE
    )
  })

  output$breeding_season_glossary_table <- renderReactable({
    breeding_season_glossary_table |>
      reactable(
        columns = season_glossary_col_styles,
        defaultColDef = colDef(maxWidth = 250)
      )
  })

  #block effort map
  output$block_effort_map <- renderMaplibre({
    block_summary <- block_summary |>
      filter(season == input$season_variable) |>
      collect()

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
      arrange(block_region, block_name) |>
      collect() |>
      reactable(
        resizable = TRUE,
        columns = list(
          pba3_block = colDef(
            name = "Block ID",
            filterable = TRUE,
            minWidth = 150,
            cell = function(value) {
              url <- paste0("https://ebird.org/atlaspa/block/", value)
              tags$a(href = url, target = "_blank", value)
            }
          ),
          block_name = colDef(
            name = "Block name",
            filterable = TRUE,
            minWidth = 220
          ),
          block_region = colDef(
            name = "Block region",
            filterable = TRUE,
            minWidth = 180
          ),
          season = colDef(name = "Season", minWidth = 100),
          checklist_count = colDef(name = "Checklists", minWidth = 100),
          species_observed = colDef(name = "Total species", minWidth = 120),
          birders = colDef(name = "Atlasers", minWidth = 100),
          duration_hours_total = colDef(
            name = "Total",
            format = colFormat(digits = 2),
            minWidth = 150
          ),
          duration_hours_diurnal = colDef(
            name = "Diurnal",
            format = colFormat(digits = 2),
            minWidth = 150
          ),
          duration_hours_nocturnal = colDef(
            name = "Nocturnal",
            format = colFormat(digits = 2),
            minWidth = 150
          ),
          duration_hours_unknown = colDef(
            name = "Unknown",
            format = colFormat(digits = 2),
            minWidth = 150
          ),
          effort_distance_km = colDef(
            name = "Effort distance (km)",
            minWidth = 175
          ),
          pct_missing_pba2_confirmations = colDef(
            name = "% of PBA2 confirmations missing",
            format = colFormat(percent = TRUE, digits = 0),
            minWidth = 300
          )
        ),
        defaultPageSize = 15,
        columnGroups = list(colGroup(
          name = "Effort hours",
          columns = c(
            "duration_hours_total",
            "duration_hours_diurnal",
            "duration_hours_nocturnal",
            "duration_hours_unknown"
          )
        ))
      )
  })

  atlas_comparison <- reactive({
    req(input$block_choice)

    missing_pba2_breeding_category_obs |>
      filter(str_detect(pba3_block, input$block_choice)) |>
      arrange(pba3_block)
  })

  output$block_atlas_comparison_table <- renderReactable({
    req(nrow(atlas_comparison()) > 0)

    atlas_comparison() |>
      reactable(
        resizable = TRUE,
        columns = list(
          pba3_block = colDef(
            name = "Block ID",
            filterable = TRUE,
            maxWidth = 150,
            cell = function(value) {
              url <- paste0("https://ebird.org/atlaspa/block/", value)
              tags$a(href = url, target = "_blank", value)
            }
          ),
          block_name = colDef(
            name = "Block name",
            filterable = TRUE,
            maxWidth = 220
          ),
          block_region = colDef(
            name = "Block region",
            filterable = TRUE,
            maxWidth = 220
          ),
          common_name = colDef(name = "Common Name"),
          pba3_breeding_category_max = colDef(
            name = "Max PBA3 Breeding Category"
          ),
          pba3_breeding_rank_max = colDef(name = "Max PBA3 Breeding Rank"),
          pba2_breeding_category_max = colDef(
            name = "Max PBA2 Breeding Category"
          ),
          pba2_breeding_rank_max = colDef(name = "Max PBA2 Breeding Rank")
        ),
        defaultPageSize = 15,
      )
  })

  output$readme <- renderUI({
    tags$iframe(
      seamless = "seamless",
      src = "readme.html",
      width = "100%",
      height = "100%"
    )
  })

  output$ebird_release <- renderUI({
    str_c("Data includes checklists from Jan-2024 to", ebird_release, sep = " ")
  })

  #logic for block report export
  #not in scope right now due to memory contraint in free tier of Posit Connect Cloud
  # report_filename <- reactive({
  #   paste0(
  #     "pba3_atlas_block_report_",
  #     input$report_block_id,
  #     "_",
  #     input$report_season,
  #     ".",
  #     input$report_format
  #   ) |>
  #     str_replace_all(" ", "_")
  # })

  # output$download_report <- downloadHandler(
  #   filename = \() {
  #     report_filename()
  #   },
  #   content = \(file) {
  #     project_dir <- normalizePath(".")
  #     report_path <- file.path(project_dir, "reports", "block_report.qmd")
  #     report_dir <- dirname(report_path)
  #     out_name <- report_filename()
  #     out_path <- file.path(report_dir, out_name)

  #     old <- setwd(project_dir)
  #     on.exit(setwd(old), add = TRUE)

  #     quarto::quarto_render(
  #       input = report_path,
  #       output_format = input$report_format,
  #       output_file = out_name,
  #       execute_params = list(block_id = input$report_block_id, season = input$report_season),
  #       execute_dir = project_dir,
  #       quiet = FALSE
  #     )

  #     file.copy(out_path, file, overwrite = TRUE)

  #     if (dir.exists(file.path(report_dir, "block_report_files"))) {
  #       unlink(file.path(report_dir, "block_report_files"), recursive = TRUE)
  #     }

  #     if (file.exists(out_path)) {
  #       unlink(out_path)
  #     }
  #   }
  # )

  bird_df_summary <- reactive({
    block_summary |>
      filter(
        pba3_block == input$report_block_id,
        season == input$report_season
      ) |>
      collect() |>
      st_drop_geometry()
  })

  ebd_df_summary <- reactive({
    seasons_filtered <- seasons |>
      filter(season == input$report_season)

    ebd_df |>
      filter(pba3_block == input$report_block_id) |>
      collect() |>
      semi_join(seasons_filtered, by = c("observation_month" = "month"))
  })

  output$summary_effort <- render_gt({
    summarize_effort(bird_df_summary())
  })

  output$summary_breeding_codes <- render_gt({
    summarize_breeding_codes(bird_df_summary())
  })

  output$summary_checklist_map <- renderMaplibre({
    ebd_df_summary() |>
      distinct(checklist_id, longitude, latitude) |>
      count(longitude, latitude, name = "checklist_count") |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
      map_checklist_count()
  })
}
