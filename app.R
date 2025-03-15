library(shiny)
library(bslib)
library(tidyverse)
library(reactable)
library(shinyWidgets)

breeding_calendar_raw <- read_csv("inputs/breeding_calendar.csv")

breeding_calendar_long <- breeding_calendar_raw |> 
  mutate(across(everything(), \(x)replace_na(x,  ""))) |> 
  select(-taxonomic_order) |> 
  pivot_longer(cols = -c(common_name, priority_species),
               names_to = "week",
               values_to = "breeding_calendar_code") |> 
  mutate(year_week_index = row_number(),
         .by = common_name)

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
      list(background = color,
           alpha = .5)
    }
  )
}

glossary_table <- tibble(
  Code = c("B", "E", "M", "N"),
  Description = c("Breeding season only", "Either migration or breeding", "Migration", "Non-breeding season")
)

#formatting for glossary
glossary_cols <- names(glossary_table)

glossary_col_styles <- map(glossary_cols, breeding_color_formatting)

names(glossary_col_styles) <- glossary_cols

safe_dates <- read_csv("inputs/bird_safe_dates.csv") |> 
  select(common_name, safe_date_probable_start, safe_date_probable_end, safe_date_possible_start, safe_date_possible_end) |> 
  arrange(safe_date_probable_start)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  current_month <- month(Sys.Date(), label = TRUE, abbr = TRUE) |> as.character()
  
  breeding_calendar <- eventReactive(input$toggle_current_month, {
    
    x <- if(input$toggle_current_month) {
      
      current_month_index <- year_week_index_df |>
        filter(str_detect(week, current_month)) |>
        slice_head(n = 1) |>
        pull(year_week_index)
      
      breeding_calendar_long |>
        left_join(year_week_index_df) |> 
        filter(year_week_index >= current_month_index) |> 
        select(-year_week_index)
      
    } else if(input$toggle_current_month == FALSE) {breeding_calendar_long}
    
    x |>
      pivot_wider(names_from = week, values_from = breeding_calendar_code)
    
  })
  
  breeding_table_formatting <- reactive({
    
    #formatting for calendar
    month_cols <- breeding_calendar() |> 
      select(-c(1:2)) |> 
      names()
    
    breeding_col_styles <- map(month_cols, breeding_color_formatting)
    
    names(breeding_col_styles) <- month_cols
    
    other_cols <- list(common_name = colDef("Common Name",
                                            filterable = TRUE,
                                            sticky = "left",
                                            style = list(borderRight = "2px solid #eee"),
                                            headerStyle = list(borderRight = "1px solid #eee")),
                       priority_species = colDef("Priority",
                                                 filterable = TRUE))
    
    c(other_cols, breeding_col_styles)
    
  })
  
  output$calendar <- renderReactable({
    
    breeding_calendar() |>    
      reactable(columns = breeding_table_formatting())
    
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
    
  ),
  
  nav_panel(
    
    "Settings",
    
    materialSwitch(inputId = "toggle_current_month", label = "Start on current month", value = TRUE)#,
    #materialSwitch(inputId = "toggle_safe", label = "Safe", value = TRUE),
    #materialSwitch(inputId = "toggle_probable", label = "Probable", value = FALSE)
    
  )
)

shinyApp(ui = ui, server = server)