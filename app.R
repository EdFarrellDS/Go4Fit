# Author: Ed Farrell
# Student Number: 28629396
# Email: efar0002@student.monash.edu.au
# Working date: 25/08/2018
# ShinyApps.io link: N/A


## map_app.R ##
# Mapping for IE project Go4Fit

############################
##  Installs & Libraries  ##
############################

library(shiny)                                        # Duh, it's a Shiny platform
#library(devtools)                                    # Installing from github
library(leaflet)                                      # Map display
#devtools::install_github('hadley/ggplot2')
#devtools::install_github('ropensci/plotly')
#library(plotly)                                       # Interactive plotting
#library(treemapify)                                   # Builds the treemap visualisation
library(dplyr)                                        # Mainly for piping; will break EVERYTHING if removed
library(tidyr)                                        # Cleaning
#library(DT)                                           # Dataframe display for treemap page


############################
##    Load Data & CWD     ##
############################

# Assumes the site_data is in the same directory as app.R, and RStudio is being used as the IDE
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
site_data <- read.csv("cleanedSportandRec.csv", stringsAsFactors=FALSE)
vic_latlong <- c(-37.8136, 144.9631)
vic_bounds <- c(-33.75, -39.25, 140.5, 152.5)
zoom_max <- 7.5



############################
##  Build text variables  ##
############################

# Declaring text variables for treemap discussion
map_text1 <- paste("Feel free to start searching for all registered physical activity and sports facilities around Victoria.")
map_text2 <- paste("You can search for locations by first entering the Region (or Council), followed by selecting any of the suburbs covered by that region, and finally selecting the sport or activity that you're interested in. To clear your selections, simply click in the list you wish to clear, and press backspace.")


############################
##      ui.R service      ##
############################

ui <- fluidPage(
  fluidRow(column(leafletOutput("map", width="150%", height=600), width=8)),
  fluidRow(
    column(selectInput("lga_choice", label="Select Region(s):", multiple=T, choices=c(unique(site_data$LGA)),selected="Melbourne"), width=3),
    column(uiOutput("suburb_choice"), width=3),
    column(uiOutput("sport_choice"), width=4)
  ),
  br(),
  fluidRow(
    #column(HTML(paste(h5(map_text1), h5(map_text2))), width=10)
  )
)


############################
##    server.R service    ##
############################

# Initialise the server
server <- function(input, output) {

  ############################
  ##  Build sub-dataframes  ##
  ############################
  
  
  ############################
  ##  Declare server funcs  ##
  ############################
 
  # Suburb choice input UI element
  output$suburb_choice <- renderUI({
    selectInput("suburb_choice", label="Select Suburb(s):", multiple=T,
                choices=c(unique(site_data[site_data$LGA == input$lga_choice,]$SuburbTown)),
                selected="Melbourne")
  })
  output$sport_choice <- renderUI({
    selectInput("sport_choice", label="Select Sport(s):", multiple=T,
                choices=c(unique(site_data[site_data$SuburbTown == input$suburb_choice,]$SportsPlayed))
                )
  })
 
  # World map plot
  # STATIC: Set the base map using leaflet, and then redraw points reactively using leafletProxy. Defaults to 1970, all attacks.
  output$map <- renderLeaflet({
    vis_data <- site_data
    leaflet(site_data, options=leafletOptions(minZoom=zoom_max)) %>%
      #addProviderTiles(providers$HERE.normalDay) %>% 
      addTiles(options=tileOptions(noWrap=F)) %>%
      addCircleMarkers(lat=vis_data$Latitude, lng=vis_data$Longitude,
                       popup=paste("Site:", vis_data$FacilityName, "</br>", "Suburb:", vis_data$SuburbTown, "</br>",
                                   "Sport:", vis_data$SportsPlayed, "</br>"),
                       clusterOptions=markerClusterOptions(spiderfyDistanceMultiplier=1.75),
                       radius = 6,
                       color = "red",
                       fillOpacity=0.5,
                       stroke=F) %>%
      setView(lat=vic_latlong[1], lng=vic_latlong[2]+0.25, zoom=zoom_max) %>%
      setMaxBounds(lat1=vic_bounds[1], lat2=vic_bounds[2], lng1=vic_bounds[3], lng2=vic_bounds[4])
  })
  # REACTIVE: update the map visualisation on action
  observeEvent(
    # Set the events that will trigger an update (can handle multiple actors)
    c(input$lga_choice,
    input$sport_choice,
    input$suburb_choice),
    # Declare the outcome of the observeEvent updates
    {SUBURB = input$suburb_choice
    SPORT = input$sport_choice
    LGA = input$lga_choice

    vis_data <- site_data[site_data$SportsPlayed %in% SPORT &
                            site_data$LGA %in% LGA &
                            site_data$SuburbTown %in% SUBURB,]
    
    
    # REACTIVE: Reset the plot based on inputs.
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearPopups() %>%
      clearShapes() %>%
      #addProviderTiles(providers$HERE.normalDay) %>% 
      addMarkers(lat=vis_data$Latitude, lng=vis_data$Longitude,
                       popup=paste("Site:", vis_data$FacilityName, "</br>", "Suburb:", vis_data$SuburbTown, "</br>",
                                   "Sport:", vis_data$SportsPlayed, "</br>"),
                       clusterOptions=markerClusterOptions(spiderfyDistanceMultiplier=1.75)
                       #radius = 6,
                       #color = "red",
                       #fillOpacity=0.5,
                       #stroke=F
                 )
    })
  
}


############################
##       Initialise       ##
############################

# Initialise the app itself
shinyApp(ui, server)