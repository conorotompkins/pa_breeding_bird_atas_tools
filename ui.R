library(shiny)
library(bslib)
library(bsicons)
library(reactable)
library(shinyWidgets)
library(mapgl)
library(gt)

ui <- page_navbar(
  fillable_mobile = TRUE,

  title = "PBA3 Tools",

  window_title = "PBA3 Tools",

  nav_spacer(),

  nav_menu(
    "Breeding season calendar",
    nav_panel(
      "Calendar",

      card(reactableOutput(outputId = "calendar"), full_screen = TRUE)
    ),

    nav_panel(
      "Safe Dates",

      card(reactableOutput("dates_table"), full_screen = TRUE)
    ),

    nav_panel(
      "Season glossary",

      reactableOutput("breeding_season_glossary_table")
    )
  ),

  nav_menu(
    "Block Progress",
    nav_panel(
      "Map",

      layout_columns(
        radioGroupButtons(
          inputId = "season_variable",
          label = "Select season",
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
        label = "Select season",
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
      textInput(inputId = "block_choice", label = "Enter Block ID"),
      reactableOutput("block_atlas_comparison_table")
    ),

    nav_panel(
      "Block report",
      layout_sidebar(
        # sidebar + main content
        sidebar = sidebar(
          open = TRUE,
          width = 300,
          title = "Generate a report for a block",

          # User chooses their 3×3 km block
          selectInput(
            inputId = "block_id",
            label = "Block ID",
            choices = c("40080D1SE")
          ),

          selectInput(
            inputId = "season",
            label = "Season",
            choices = c("All seasons")
          ),

          selectInput(
            "report_format",
            "Format",
            choices = c("html", "pdf"),
            selected = "html"
          ),
          downloadButton("download_report", "Download report"),
        ),

        card(
          full_screen = TRUE,
          card_header("Block progress report"),
          layout_columns(
            gt_output("summary_effort"),
            gt_output("summary_breeding_codes")
          ),
          maplibreOutput("summary_checklist_map")
        )
      )
    )
  ),

  nav_panel(
    "About",

    # card(
    #   card_image(
    #     file = "input/pba3_logo.svg",
    #     href = "https://ebird.org/atlaspa/home",
    #     fill = TRUE
    #   ),
    #   max_height = 100
    # ),

    card(
      uiOutput("readme")
    ),

    card_footer(
      "App developed by Conor Tompkins with assistance from Amber Wiewel and Joe Gyekis.",
      popover(
        a(
          "eBird data extracted from the eBird Basic Dataset.",
          href = "https://ebird.org/data/download"
        ),
        markdown(
          "eBird Basic Dataset. Version: EBD_relNov-2025. Cornell Lab of Ornithology, Ithaca, New York. Nov 2025."
        )
      )
    )
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
        ),
        materialSwitch(
          inputId = "toggle_show_priority_column",
          label = "Show priority column",
          value = FALSE
        )
      ),
      accordion_panel(
        value = "accordion_map",
        title = "Block map"
      )
    ),
  ),
  nav_item(
    "Release",
    tooltip(
      bs_icon("info-circle"),
      uiOutput("ebird_release")
    )
  )
)
