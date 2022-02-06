library(shiny)
library(tidyverse)
library(palmerpenguins)
library(shinythemes)

library(bslib)

# In console, run bs_theme_preview() to play around with different things!

# See ?bs_theme() for more options & information.

my_theme <- bs_theme(
  bg = "#bbccdd",
  fg = "#0f644d",
  primary = "#eeeeea",
  base_font = font_google("Raleway")
)


ui <- fluidPage(theme = my_theme,
  titlePanel("Meso-American Reef Watershed Basin Impacts"),
  sidebarLayout(
    sidebarPanel("Select which areas and pollutants you'd like to investigate:",
                 selectInput("select", label = h5("Region"),
                             choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                             selected = 1),
                 dateRangeInput("dates",
                                label = h5("Date Range")
                                ),
                 checkboxGroupInput(inputId = "country_check", label = h5("Country"),
                                    choices = c("Mexico" = "Mexico", 
                                                "Honduras" = "Honduras", 
                                                "Guatemala" = "Guatemala",
                                                "Belize" = "Belize")),
                 radioButtons("radio", label = h5("Pollutant"),
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                              selected = 1),
                 
    ), 
    #end widgets
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Graph", plotOutput(outputId = "ma_reef")),
                  tabPanel("Table", tableOutput("table"))
      ) # end main panel
    ) # end sidebarLayout
))

server <- function(input, output) {
  country_select <- reactive({
    basins %>% 
      filter(country_c_80 == input$country_check)
  }) #end country check reactive
  
  output$value <- renderPrint({input$dates})
  
  output$ma_reef <- renderPlot({
    ggplot(data = country_select(), aes(x = crops_n_n_24_15, y = lvstc_n_n_24_15)) +
      geom_point()
  })
}

shinyApp(ui = ui, server = server)