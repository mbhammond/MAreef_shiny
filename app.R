library(shiny)
library(tidyverse)
library(palmerpenguins)


ui <- fluidPage(
  titlePanel("Title Here"),
  sidebarLayout(
    sidebarPanel("widgets here",
                 dateRangeInput("dates",
                                label = h5("Date Range")
                                ),
                 checkboxGroupInput("checkGroup", label = h5("Checkbox group"),
                                    choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                    selected = 1),
                 radioButtons("radio", label = h5("Radio Buttons"),
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                              selected = 1)
    ), 
    #end widgets
    mainPanel("Graph here")
    )
)




server <- function(input, output) {
  output$value <- renderPrint({input$dates})
}

shinyApp(ui = ui, server = server)