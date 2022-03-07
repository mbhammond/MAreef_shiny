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
library(ggbeeswarm)
library(DT)

# -----------------------
### all data input
cruise_pulse <- read_csv(here("data", "cruise_pulse_all_info.csv")) %>% clean_names()

reef_area <- read_csv(here("data", "reef_area_exposure.csv")) %>% clean_names()

seagrass_area <- read_csv(here("data", "seagrass_area_exposure.csv")) %>% clean_names()

basins <- read_csv(here("data", "basins_admin.csv")) %>% clean_names()

basins_long <- basins %>% pivot_longer(res_n_n_24_15:lvstc_n_n_24_15, names_to = "source", values_to = "n_quantity")

watersheds <- read_csv(here("data", "top_20_watersheds.csv")) %>% clean_names()

reef_admin <- read_csv(here("data", "reef_admin.csv")) %>% clean_names()

### map input
basins_sf <- read_sf(here('data/spatial/basins_admin.shp'))

basins_sf_long <- basins_sf %>% pivot_longer(res_N:lvstc_N, names_to = "source", values_to = "N_quantity")

top_watershed_sf <- read_sf(here('data/spatial/top_20_watersheds.shp'))


basins_long_graph <- basins_long %>% 
  group_by(admn_bn_c_80, source, country_c_80) %>%
  summarize(n_total = sum(n_quantity))

reef_admin_edit <- reef_admin %>% 
  group_by(name_c_80, country_c_80) %>% 
  summarize(n_area_total = sum(area_km2_n_24_15))

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
                                      sidebarPanel("Select which Nitrogen source you would like to investigate:",
                                                   radioButtons("pollutant_check", label = h5("Nitrogen Source"),
                                                                choices = list(Residential = "res_N",
                                                                               Tourists = "torst_N",
                                                                               Crops = "crops_N",
                                                                               Livestock = "lvstc_N"),
                                                                selected = "res_N"),
                                                   
                                      ),
                                      #end widgets
                                      mainPanel(
                                        plotlyOutput(outputId = "ma_reef_map")
                                        # tabsetPanel(type = "tabs",
                                        #             tabPanel("Map", plotlyOutput(outputId = "ma_reef_map"))
                                        # ) # end main panel
                                      )) # end sidebarLayout
                           ),
                           tabPanel("Graphs",
                                    titlePanel("Meso-American Reef Watershed Basin Impacts"),
                                    sidebarLayout(
                                      sidebarPanel("Select which areas and pollutants you'd like to investigate:",
                                                   checkboxGroupInput(inputId = "country_check", label = h5("Country"),
                                                                      choices = c("Mexico" = "Mexico",
                                                                                  "Honduras" = "Honduras",
                                                                                  "Guatemala" = "Guatemala",
                                                                                  "Belize" = "Belize"),
                                                                      selected = "Mexico"),
                                                   radioButtons("pollutant_check_2", label = h5("Nitrogen Source"),
                                                                choices = list(Residential = "res_n_n_24_15",
                                                                               Tourists = "torst_n_n_24_15",
                                                                               Crops = "crops_n_n_24_15",
                                                                               Livestock = "lvstc_n_n_24_15"),
                                                                selected = "res_n_n_24_15")
                                      ),
                                      #end widgets
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Nitrogen Effluents by Watershed", plotlyOutput(outputId = "ma_reef")),
                                                    tabPanel("Total Nitrogen Effluent by Country", tableOutput(outputId = "ma_reef_tab"))
                                        ) # end main panel
                                      )) # end sidebarLayout
                           ),
                           tabPanel("Reef Impact",
                                    titlePanel("Meso-American Reef Watershed Basin Impacts"),
                                    sidebarLayout(
                                      sidebarPanel("Select the range of reef area that is affected by Nitrogen pollution that you would like to investigate:",
                                                   sliderInput("nitrogen_area", label = h3("Nitrogen Affected Reef Area"),
                                                               min = 0,
                                                               max = 250,
                                                               value = c(100, 130))
                                                   
                                        
                                      ), #end widgets
                                      mainPanel(
                                        dataTableOutput(outputId = "n_reef_area")
                                        # tabsetPanel(type = "tabs",
                                        #             tabPanel("Nitrogen Affected Reef Area", dataTableOutput(outputId = "n_reef_area")))
                                        # 
                                      ) 
                                    ) 
                                
                                    )))

server <- function(input, output) {
  
  # Slider nitrogen graph
#  nitrogen_reef_area <- reactive({
 #   reef_admin_edit %>% 
  #    filter(n_area_total )
 # })
  
  # reactive output for selected countries
  country_select_long <- reactive({
    basins_long %>%
      filter(country_c_80 %in% input$country_check)
  }) #end country check reactive
  
  country_select_long_2 <- reactive({
    basins_long_graph %>%
      filter(country_c_80 %in% input$country_check,
             source == input$pollutant_check_2) #%>%
     # group_by(admn_bn_c_80) %>% 
     # summarize(total_n_basin = sum(n_quantity))
     # filter(source == input$pollutant_check_2)
     # filter(n_quantity > 0)
  })
  
  country_select <- reactive({
    basins %>%
      filter(country_c_80 %in% input$country_check)
  })
  
  # reactive output for selected nitrogen source
  map_reactor <- reactive({
    basins_sf_long %>%
      filter(source == input$pollutant_check)
  })
  
  # pollutant_reactor <- reactive({
  #   basins_long %>% 
  #     filter(source == input$pollutant_check_2,
  #            country_c_80 %in% input$country_check)
  # })
  
  # ???
  output$value <- renderPrint({input$dates})
  
  # plot output 1
  output$ma_reef <- renderPlotly({
 #   country_select_long_2 %>% 
  #    group_by(admn_bn_c_80) %>% 
      ggplotly(
          ggplot(data = country_select_long_2(),
           aes(x = admn_bn_c_80, y = n_total)) +
      geom_col(aes(fill = admn_bn_c_80)) +
      facet_grid(~country_c_80, scales = "free") +
      coord_flip() +
      theme_minimal() +
        theme(legend.position = "none")
      )
  })
  
  # table output 1
  output$ma_reef_tab <- renderTable({
    country_select_long() %>%
      group_by(country_c_80) %>%
      summarize(total_n = sum(n_quantity))
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
  
  # output Nitrogren reef area
  output$n_reef_area <- DT::renderDataTable({
    dt <- reef_admin_edit[reef_admin_edit$n_area_total >= input$nitrogen_area[1] & reef_admin_edit$n_area_total <= input$nitrogen_area[2], ]
  })
  
}

shinyApp(ui = ui, server = server)
