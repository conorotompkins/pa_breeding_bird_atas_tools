library(shiny)
library(bslib)
library(tidyverse)
library(reactable)

breeding_calendar <- read_csv("inputs/breeding_calendar.csv")

month_cols <- breeding_calendar |> 
  select(-c(1:3)) |> 
  names()

breeding_calendar <- breeding_calendar |> 
  mutate(across(all_of(month_cols), \(x)replace_na(x,  ""))) |> 
  select(-taxonomic_order)

glossary_table <- tibble(
  Code = c("B", "E", "M", "N"),
  Description = c("Breeding season only", "Either migration or breeding", "Migration", "Non-breeding season")
)

#formatting for calendar
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
        alpha = .5)
      }
    )
}

breeding_col_styles <- map(month_cols, breeding_formatting)

names(breeding_col_styles) <- month_cols

other_cols <- list(common_name = colDef("Common Name",
filterable = TRUE,
sticky = "left",
style = list(borderRight = "2px solid #eee"),
      headerStyle = list(borderRight = "1px solid #eee")),
priority_species = colDef("Priority",
filterable = TRUE))

breeding_table_formatting <- c(other_cols, breeding_col_styles)

#formatting for glossary
glossary_cols <- names(glossary_table)

glossary_col_styles <- map(glossary_cols, breeding_formatting)

names(glossary_col_styles) <- glossary_cols

safe_dates <- read_csv("inputs/bird_safe_dates.csv") |> 
  select(common_name, safe_date_probable_start, safe_date_probable_end, safe_date_possible_start, safe_date_possible_end) |> 
  arrange(safe_date_probable_start)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$calendar <- renderReactable({   
    
    breeding_calendar |>   
      reactable(columns = breeding_table_formatting)

})
  
  output$dates_table <- renderReactable({

    reactable(safe_dates,

      columns = list(

        common_name = colDef(name = "Common Name"),
        safe_date_probable_start = colDef(name = "Probable Start"),
        safe_date_probable_end = colDef(name = "Probable End"),
        safe_date_possible_start = colDef(name = "Possible Start"),
        safe_date_possible_end = colDef(name = "Possible End"))
      )

  })

  output$glossary_table <- renderReactable({

    glossary_table |>
      reactable(columns = glossary_col_styles)

  })
}

ui <- page_navbar(

  fillable_mobile = TRUE,

  title = "Breeding Bird Calendar",

  nav_panel(
        
        "Calendar",

        card(reactableOutput(outputId = "calendar"))

      ),

      nav_panel("Safe/Probable dates",
    
      card(reactableOutput("dates_table"))

    ),

      nav_panel("Glossary",

      reactableOutput("glossary_table")
   
    ),

      nav_panel(
        
        "About",

        tags$a(href="https://ebird.org/atlaspa/home", "Pennsylvania Bird Atlas 3",  target="_blank"),

        p("App developed by Conor Tompkins")
      
      )
    )

shinyApp(ui = ui, server = server)