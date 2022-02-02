library(shiny)
library(tidyverse)
library(palmerpenguins)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("NOT TOO SHABBY AY"),
  sidebarLayout(
    sidebarPanel("widgets here",
                 selectInput("select", label = h5("Select box"),
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
                 radioButtons("radio", label = h5("Radio Buttons"),
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                              selected = 1),
                 
    ), 
    #end widgets
    mainPanel("Graph here",
              plotOutput(outputId = "ma_reef")
              ) # end main panel
    ) # end sidebarLayout
)




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