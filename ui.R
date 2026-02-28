library(shiny)
library(bslib)
library(reactable)
library(shinyWidgets)
library(mapgl)

ui <- page_navbar(
  fillable_mobile = TRUE,

  title = "PBA3 Tools",

  window_title = "PBA3 Tools",

  nav_spacer(),

  nav_menu(
    "Breeding Calendar",
    nav_panel(
      "Calendar",

      card(reactableOutput(outputId = "calendar"))
    ),

    nav_panel(
      "Safe Dates",

      card(reactableOutput("dates_table"))
    ),

    nav_panel(
      "Code Glossary",

      reactableOutput("glossary_table")
    )
  ),

  nav_menu(
    "Block Progress",
    nav_panel(
      "Map",

      layout_columns(
        radioGroupButtons(
          inputId = "season_variable",
          label = "Season",
          choices = c(
            "All seasons" = "All seasons",
            "Breeding" = "Breeding",
            "Winter" = "Winter"
          )
        ),
        radioGroupButtons(
          inputId = "block_variable",
          label = "Variable",
          choices = c(
            "Effort hours" = "duration_hours",
            "Confirmed species" = "Confirmed"
          )
        )
      ),
      maplibreOutput("block_effort_map")
    ),

    nav_panel(
      "Table",

      radioGroupButtons(
        inputId = "season_variable_table",
        label = "Season",
        choices = c(
          "All seasons" = "All seasons",
          "Breeding" = "Breeding",
          "Winter" = "Winter"
        )
      ),
      reactableOutput("block_completion_table")
    ),

    nav_panel(
      "Atlas Comparison Table",
      textInput(inputId = "block_choice", label = "Choose Block"),
      reactableOutput("block_atlas_comparison_table")
    )
  ),

  nav_panel(
    "About",

    tags$a(
      href = "https://ebird.org/atlaspa/home",
      "Third Pennsylvania Bird Atlas",
      target = "_blank"
    ),

    p("App developed by Conor Tompkins")
  ),

  nav_panel(
    "Settings",

    accordion(
      accordion_panel(
        value = "accordion_calendar",
        title = "Calendar",
        materialSwitch(
          inputId = "toggle_current_month",
          label = "Start on current month",
          value = TRUE
        ),
        materialSwitch(
          inputId = "toggle_exclude_na_code",
          label = "Exclude birds with no code in first month",
          value = TRUE
        )
      ),
      accordion_panel(
        value = "accordion_map",
        title = "Block map"
      )
    ),
  )
)
