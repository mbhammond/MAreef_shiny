library(shiny)
library(tidyverse)
library(palmerpenguins)
library(shinythemes)
library(here)
library(bslib)
library(devtools)
library(tmap)
library(janitor)
library(sf)
library(plotly)

# -----------------------
  ### all data input
  cruise_pulse <- read_csv(here("data", "cruise_pulse_all_info.csv")) %>% clean_names()
  
  reef_area <- read_csv(here("data", "reef_area_exposure.csv")) %>% clean_names()
  
  seagrass_area <- read_csv(here("data", "seagrass_area_exposure.csv")) %>% clean_names()
  
  basins <- read_csv(here("data", "basins_admin.csv")) %>% clean_names() 
  
  watersheds <- read_csv(here("data", "top_20_watersheds.csv")) %>% clean_names()
  
  reef_admin <- read_csv(here("data", "reef_admin.csv")) %>% clean_names()
  
 ### map input 
  basins_sf <- read_sf(here('data/spatial/basins_admin.shp'))
  
  basins_sf_long <- basins_sf %>% pivot_longer(res_N:lvstc_N, names_to = "source", values_to = "N_quantity")
  
  top_watershed_sf <- read_sf(here('data/spatial/top_20_watersheds.shp'))
  
  
  
# ---------------------------------------

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
                           tabPanel("Maps",
  titlePanel("Meso-American Reef Watershed Basin Impacts"),
  sidebarLayout(
    sidebarPanel("Select which areas and pollutants you'd like to investigate:",
                 selectInput("select", label = h5("Region"),
                             choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                             selected = 1),
                 radioButtons("pollutant_check", label = h5("Nitrogen Source"),
                              choices = list(res_N = "res_N", 
                                             torst_N = "torst_N", 
                                             crops_N = "crops_N",
                                             lvstc_N = "lvstc_N"),
                              selected = "res_N"),

    ), 
    #end widgets
    mainPanel(
      tabsetPanel(plotlyOutput(outputId = "ma_reef_map"))
      ) # end main panel
    )) # end sidebarLayout
  ),
  tabPanel("Graphs",
         titlePanel("Meso-American Reef Watershed Basin Impacts"),
         sidebarLayout(
           sidebarPanel("Select which areas and pollutants you'd like to investigate:",
                        dateRangeInput("dates",
                                       label = h5("Date Range")
                        ),
                        checkboxGroupInput(inputId = "country_check", label = h5("Country"),
                                           choices = c("Mexico" = "Mexico", 
                                                       "Honduras" = "Honduras", 
                                                       "Guatemala" = "Guatemala",
                                                       "Belize" = "Belize"),
                                           selected = "Mexico"),
           ), 
           #end widgets
           mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel("Graph", plotOutput(outputId = "ma_reef")),
                         tabPanel("Table", tableOutput(outputId = "ma_reef_tab"))
             ) # end main panel
           )) # end sidebarLayout
         ))

server <- function(input, output) {
  
  # reactive output for selected countries
  country_select <- reactive({
    basins %>% 
      filter(country_c_80 == input$country_check)
  }) #end country check reactive
  
  # reactive output for selected nitrogen source
    map_reactor <- reactive({
     basins_sf_long %>%
       filter(source == input$pollutant_check)
  })
  
  # ???
  output$value <- renderPrint({input$dates})
  
  # plot output 1
  output$ma_reef <- renderPlot({
    ggplot(data = country_select(), aes(x = crops_n_n_24_15, y = lvstc_n_n_24_15)) +
      geom_point() +
      theme_minimal()
  })

  # table output 1
  output$ma_reef_tab <- renderTable({
    country_select() %>%
      group_by(country_c_80) %>%
      summarize(total_n = sum(area_n_24_15))
  })
  
  # map output 1
  output$ma_reef_map <- renderPlotly({
    ggplotly(
      ggplot() +
      geom_sf(data = top_watershed_sf) +
      geom_sf(data = map_reactor(), aes(fill = map_reactor()$N_quantity,
                                        text = paste("Basin ID: ",
                                                     map_reactor()$admn_bn,
                                                     "<br>Source: ", map_reactor()$source,
                                                      "<br>Nitrogen: ", 
                                                      map_reactor()$N_quantity,
                                                      " UNITS")
                                        )) + #change back to just data=basins_sf? no!
      theme_minimal()
    )
  })
  

}

shinyApp(ui = ui, server = server)