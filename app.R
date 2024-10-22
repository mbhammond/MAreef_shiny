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
  summarize(n_area_total = round(sum(area_km2_n_24_15), 0))

### color palettes
# Livestock:
lives_pal <- c("#E3AA64","#E8B87D","#ECC696","#F1D5AF","#F6E3C7","#FAF1E0","#FFFFF9")

# Residential:
res_pal <- c("#015653","#266F6D","#4B8887","#71A1A1","#96BABA","#BBD3D4","#E0ECEE")

# Crops:
crops_pal <- c("#F4EB89","#F5ED99","#F6EFAA","#F7F2BA","#F7F4CA","#F8F6DB","#F9F8EB")

# Tourism:
tourism_pal <- c("#7064A6","#867AB4","#9C90C2","#B2A6D0","#C7BBDE","#DDD1EC","#F3E7FA")
  
# DF of color palettes
colors <- rbind.data.frame(lives_pal, res_pal, crops_pal, tourism_pal)
#colors[1] = livestock
#colors[2] = residential
#colors[3] = crops
#colors[4] = tourism


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
                navbarPage(emo::ji("ocean"),
                           tabPanel("Introduction",
                                    titlePanel(h2("Mesoamerican Reef Watershed Basin Impacts", align = "center")),
                                    fluidRow(column(3,
                                                    img(src = "NCEAS-Stacked-4C_0.png", width = "275px"),
                                                    br(),
                                                    img(src = "mesoreef.png", width = "275px")
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
                                         p("Here, we model four major sources of N pollution – crop production,
                                         livestock production, wastewater generated from permanent residents and
                                         wastewater generated from seasonal populations – at the regional scale,
                                         measuring inputs and impacts from 430 watersheds that drain into the
                                         Mesoamerican Reef region. We aim to answer the question of what is the relative 
                                           contribution of different sources of excess N in coastal waters, and how do 
                                           these differences vary spatially? And how exposed are high biodiversity habitats to N pollution?"),
                                         p("Now go ahead and click on the above tabs to play around with the different countries and pollutants!"),
                                         p("Data Citation: "),
                                         p("Berger, M. (2022),",  em('Nitrogen Impacts on the Mesoamerican Reef Region'), 
                                           "[Manuscript submitted for publication]. National Center for Ecological Analysis & Synthesis, UC Santa Barbara"),
                                         br(),
                                         p(em("Developed by Sarah Hamilton and Margaret Hammond"), style = "text-align:center")
                                    ),
                                    column(3,
                                           br(),
                                           img(src = "MAR_map.png", width = "275px")
                                           )
                                    )),
                           tabPanel("Map of Watershed Basin",
                                    titlePanel("Mesoamerican Reef Watershed Basin Impacts"),
                                    sidebarLayout(
                                      sidebarPanel("Select which nitrogen source you would like to investigate:",
                                                   radioButtons("pollutant_check", label = h5("Nitrogen Source"),
                                                                choices = list(Residential = "res_N",
                                                                               Tourists = "torst_N",
                                                                               Crops = "crops_N",
                                                                               Livestock = "lvstc_N"),
                                                                selected = "res_N"),
                                                   
                                      ),
                                      #end widgets
                                      mainPanel(
                                        plotlyOutput(outputId = "ma_reef_map"),
                                        br(),
                                        h5("Mesoamerican Reef Effluent Pollution"),
                                        br(),
                                        p("The Mesoamerican Reef (MAR) spans across the coastlines of four countries – 
                                        Mexico, Belize, Guatemala and Honduras – and is the largest continuous barrier reef 
                                        in the Western Hemisphere, extending over 1000 km with four off-shelf atolls. However, 
                                        the barrier reef is a fragile ecosystem, considered critically endangered and at high risk 
                                        of extreme degradation, with ocean acidification, hurricanes, pollution and fishing the primary threats. The 
                                        region supports a robust and growing tourism industry, which includes resorts, watersport and cruise ship tourism, 
                                        all of which are reliant upon healthy coastal ecosystems. The region contains 430 watersheds, with the 
                                        southern half containing systems that are surface-water driven, and the northern half dominated by 
                                        ground-water driven systems. For all datasets, we used the most recent year available."),
                                       br(), 
                                       p("Nitrogen effluent resulting from crop and livestock production was modeled using global spatially resolved 
                                        models that estimate tons of nitrogen leached per year from the production 
                                        of 27 different crops and 20 different livestock types."),
                                         br(),
                                        p("To model nitrogen effluent from tourism and resident, wastewater was measured by combining population estimates of permanent residents or  
                                        tourists visiting the MAR region with per capita protein consumption and the proportion of the 
                                        population’s access to different wastewater treatment facilities (sewer systems, septic systems, or 
                                        open defecation).")
                                        
                                        # tabsetPanel(type = "tabs",
                                        #             tabPanel("Map", plotlyOutput(outputId = "ma_reef_map"))
                                        # ) # end main panel
                                      )) # end sidebarLayout
                           ),
                           tabPanel("Pollutant Source",
                                    titlePanel("Mesoamerican Reef Watershed Basin Impacts"),
                                    sidebarLayout(
                                      sidebarPanel("Select which countries and nitrogen source you'd like to investigate:",
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
                                                    tabPanel("Nitrogen Effluents by Watershed", 
                                                             plotlyOutput(outputId = "ma_reef"), br(), 
                                                             h5("Nitrogen Effluent by Country and Watershed Basin"),
                                                             br(),
                                                             "A distinct north-south divide was observed in N effluent estimates from all 
                                                             four sources modeled, with Guatemalan and Honduran watersheds estimated to 
                                                             have higher N effluent than watersheds in Mexico and Belize. While N pollution from 
                                                             wastewater from residents and tourists was relatively low across the region, the 
                                                             watersheds located in Guatemala and Honduras accounted for approximately 72% of 
                                                             all wastewater input. Other watersheds associated with the highest level of N 
                                                             pollution from tourism wastewater were primarily nearby tourist areas of the MAR 
                                                             and thus near high diversity habitats such as coral reefs, for example Cancun, 
                                                             Mexico, Belize City, Belize, and Bay Islands, Honduras."),
                                                    tabPanel("Total Nitrogen Effluent by Country", 
                                                             tableOutput(outputId = "ma_reef_tab"), br(), 
                                                             h5("Nitrogen Effluent by Country and Watershed Basin"),
                                                             br(),
                                                             "A distinct north-south divide was observed in N effluent estimates from all 
                                                             four sources modeled, with Guatemalan and Honduran watersheds estimated to 
                                                             have higher N effluent than watersheds in Mexico and Belize. While nitrogen pollution from 
                                                             wastewater from residents and tourists was relatively low across the region, the 
                                                             watersheds located in Guatemala and Honduras accounted for approximately 72% of 
                                                             all wastewater input. Other watersheds associated with the highest level of N 
                                                             pollution from tourism wastewater were primarily nearby tourist areas of the MAR 
                                                             and thus near high diversity habitats such as coral reefs, for example Cancun, 
                                                             Mexico, Belize City, Belize, and Bay Islands, Honduras.")
                                        ) # end main panel
                                      )) # end sidebarLayout
                           ),
                           tabPanel("Reef Impact",
                                    titlePanel("Mesoamerican Reef Watershed Basin Impacts"),
                                    sidebarLayout(
                                      sidebarPanel("Select the range of reef area that is affected by Nitrogen pollution that you would like to investigate:",
                                                   sliderInput("nitrogen_area", label = h3("Nitrogen Affected Reef Area"),
                                                               min = 0,
                                                               max = 250,
                                                               value = c(100, 130))
                                                   
                                                   
                                      ), #end widgets
                                      mainPanel(
                                        dataTableOutput(outputId = "n_reef_area"),
                                        br(),
                                        h5("Nitrogen Reef Impact"),
                                        br(),
                                        p("Reef area impacted directly correlates with proximity to high-density tourist areas. 
                                          For example, plumes originating from Rio Ulua, Guatemala, the top watershed contributing N 
                                          pollution to the MAR, did not overlap with coral reef area because there were no coral reefs
                                          within the proximity. On the other hand, plumes from Cancún and Cozumel reached 100% of the nearby reef areas.
                                          Tourism is dependent on healthy coral reef ecosystems, and nitrogen can increase coral reef stressors such as 
                                          ocean temperatures and eutrophication. It is in the economic interest of all countries in the region to improve
                                          pollution management to reduce environmental stress to coral reefs and promote their resilience to climate change."),
                                        br(),
                                        p()
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
  
  output$value <- renderPrint({input$dates})
  
  # plot output 1
  output$ma_reef <- renderPlotly({
    ggplotly(
        ggplot(data = country_select_long_2(),
             aes(x = admn_bn_c_80, y = n_total)) +
        geom_col(aes(fill = admn_bn_c_80)) +
        facet_grid(~country_c_80, scales = "free") +
          labs(x = "Watershed Basin",
               y = "Total Nitrogen (kg N/year)",
               title = "Nitrogen by Water Basin in Selected Country") +
        coord_flip() +
        theme_minimal() +
        theme(legend.position = "none") 
    )
  })
  
  # table output 1
  output$ma_reef_tab <- renderTable({
    country_select_long() %>%
      mutate(source = case_when(
        source == "res_n_n_24_15" ~ "Residents",
        source == "torst_n_n_24_15" ~ "Tourists",
        source == "lvstc_n_n_24_15" ~ "Livestock",
        source == "crops_n_n_24_15" ~ "Crops")) %>% 
      rename(Country = country_c_80) %>% 
      group_by(Country) %>%
      summarize(Nitrogen = sum(n_quantity))
  })
  
  # map output 1
  output$ma_reef_map <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_sf(data = top_watershed_sf) +
      #  tm_basemap(c(StreetMap = "OpenStreetMap",
      #               TopoMap = "OpenTopoMap")) +
        geom_sf(data = map_reactor(), aes(fill = map_reactor()$N_quantity,
                                          text = paste("Basin ID: ",
                                                       map_reactor()$admn_bn,
                                                       "<br>Source: ", map_reactor()$source,
                                                       "<br>Nitrogen: ",
                                                       map_reactor()$N_quantity,
                                                       "kg/y")
        )) + #change back to just data=basins_sf? no!
        labs(fill = "Nitrogen (kg/year)",
             title = "Nitrogen Total within the Water Basins of Mesoamerican Reef Region") +
        theme_minimal()
      #  scale_fill_gradient(low = "#000000", high = "#111111") #DELETE THIS IF COLOR CHANGE DOESN'T WORK
    )
  })
  
  # output Nitrogren reef area
  output$n_reef_area <- DT::renderDataTable({
    dt <- reef_admin_edit[reef_admin_edit$n_area_total >= input$nitrogen_area[1] & reef_admin_edit$n_area_total <= input$nitrogen_area[2], ]}, 
    options = list(
      autoWidth = TRUE,
      info = FALSE,
      columns = list(
        list(title = ""),
        list(title = "Name of Reef"),
        list(title = "Country"),
        list(title = "Area Impacted by Nitrogen (km2)"))
    ))
  
}

shinyApp(ui = ui, server = server)
