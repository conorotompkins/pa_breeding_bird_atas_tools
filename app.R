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

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$calendar <- renderReactable({   
    
    breeding_calendar |>   
      reactable(columns = breeding_table_formatting)

})
}

ui <- page_navbar(

  title = "Breeding Bird Calendar",

  nav_panel(
        
        "Calendar",

        card(reactableOutput(outputId = "calendar"))

      ),

      nav_panel("Safe dates")

)

shinyApp(ui = ui, server = server)