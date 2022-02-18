library(shiny)
library(tidyverse)
library(palmerpenguins)
library(shinythemes)

library(bslib)
library(devtools)
#devtools::install_github("hadley/emo")

# In console, run bs_theme_preview() to play around with different things!

# See ?bs_theme() for more options & information.

my_theme <- bs_theme(
  bg = "#e9f5f8",
  fg = "#0f644d",
  primary = "#a6a6a6",
  base_font = font_google("Raleway")
)


ui <- fluidPage(theme = my_theme,
                navbarPage(emo::ji("pig2"),
                           tabPanel("Introduction",
                                    titlePanel(h2("Meso-American Reef Watershed Basin Impacts", align = "center")),
                                    fluidRow(column(3,
                                           img(src = "NCEAS-Stacked-4C_0.png", width = "300px"),
                                           br(),
                                           img(src = "mesoreef.png", width = "300px")
                                    ),
                                    column(6,
                                           h5("Welcome to the watershed effluent analysis portal!"),
                                           p("Land-based nitrogen pollution is a major threat to coastal ecosystems, 
                                         especially in tropical regions home to high biodiversity habitats such as 
                                         coral reefs and seagrass beds. The sustained addition of excess nutrients 
                                         (in the form of nitrates) to these ecosystems, which are adapted to 
                                         oligotrophic environments, disrupts ecosystem function and the ability to 
                                         provide services that support livelihoods and benefit human well-being. 
                                         Nitrogen (N) primarily originates from agricultural crop production, 
                                         livestock waste, and human sewage, as well as excretion from seabird and 
                                         feral ungulates for some small atolls and cayes."),
                                           p("Here we model four major sources of N pollution – crop production, 
                                         livestock production, wastewater generated from permanent residents and 
                                         wastewater generated from seasonal populations – at the regional scale, 
                                         measuring inputs and impacts from 430 watersheds that drain into the 
                                         Mesoamerican Reef region."),
                                           p("Now go ahead and click on 'Visualizations' and play around with the different 
                                         regions, dates, countries, and pollutants!"),
                                         p("Data Citation: "),
                                         br(),
                                         p(em("Developed by Sarah Hamilton and Margaret Hammond"), style = "text-align:center")
                                           ),
                                    column(3)
                                    )),
                           tabPanel("Visualizations",
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
                  tabPanel("Map"),
                  tabPanel("Graph", plotOutput(outputId = "ma_reef")),
                  tabPanel("Table", tableOutput(outputId = "ma_reef_tab"))
      ) # end main panel
    )) # end sidebarLayout
)))

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
  
  output$ma_reef_tab <- renderTable({
    country_select() %>%
      group_by(country_c_80) %>%
      summarize(total_n = sum(area_n_24_15))
  })
}

shinyApp(ui = ui, server = server)