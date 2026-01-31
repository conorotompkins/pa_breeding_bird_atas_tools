library(shiny)
library(bslib)
library(reactable)
library(shinyWidgets)

ui <- page_navbar(
  fillable_mobile = TRUE,

  title = "Breeding Bird Calendar",

  nav_panel(
    "Calendar",

    card(reactableOutput(outputId = "calendar"))
  ),

  nav_panel(
    "Safe dates",

    card(reactableOutput("dates_table"))
  ),

  nav_panel(
    "Glossary",

    reactableOutput("glossary_table")
  ),

  nav_panel(
    "About",

    tags$a(
      href = "https://ebird.org/atlaspa/home",
      "Pennsylvania Bird Atlas 3",
      target = "_blank"
    ),

    p("App developed by Conor Tompkins")
  ),

  nav_panel(
    "Settings",

    materialSwitch(
      inputId = "toggle_current_month",
      label = "Start on current month",
      value = TRUE
    ),
    materialSwitch(
      inputId = "toggle_exclude_na_code",
      label = "Exclude birds",
      value = TRUE
    )
    #materialSwitch(inputId = "toggle_safe", label = "Safe", value = TRUE),
    #materialSwitch(inputId = "toggle_probable", label = "Probable", value = FALSE)
  )
)
