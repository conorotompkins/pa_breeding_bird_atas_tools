library(shiny)
library(bslib)
library(reactable)
library(shinyWidgets)
library(mapgl)

ui <- page_navbar(
  fillable_mobile = TRUE,

  title = "Third Pennsylvania Bird Atlas Tools",

  nav_panel(
    "Breeding Calendar",

    card(reactableOutput(outputId = "calendar"))
  ),

  nav_panel(
    "Breeding Safe Dates",

    card(reactableOutput("dates_table"))
  ),

  nav_panel(
    "Breeding Code Glossary",

    reactableOutput("glossary_table")
  ),

  nav_panel(
    "Block Progress Map",

    card(
      radioGroupButtons(
        inputId = "block_variable",
        label = "Variable",
        choices = c(
          "Effort hours" = "duration_hours",
          "Confirmed species" = "confirmed_species"
        )
      ),
      maplibreOutput("block_effort_map")
    )
  ),

  nav_panel(
    "Block Progress Table",

    card("block_progress_table")
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
