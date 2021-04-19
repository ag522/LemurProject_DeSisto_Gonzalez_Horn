##### Load packages ----

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(sf)
##### Importing and transfroming data
data <- read.csv("872_data.csv")
data <- st_as_sf(data,
                 coords = c('long','lat'),
                 crs = 4326)
data <- data %>%
    mutate(Transect_Site2 = Transect_Site) %>%
    separate(col= Transect_Site2, into="Site", sep ="_") 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Transect Site Information"),
    
    sidebarLayout(
        
        mainPanel(
    leafletOutput(outputId="mymap")
    ),
    sidebarPanel(
    selectInput("Species","Select a Species:", 
    choices = c("Avahi_laniger","Cheirogaleus_crossleyi",
                "Eulemur_rubriventer","Propithecus_edwardsi",
                "Lepilemur_microdon","Hapalemur_griseus",
                 "Eulemur_rufifrons","Varecia_variegata",
                 "Microcebus_rufus" ), selected = "Avahi_laniger")
    )
    ))
        


# Define server logic required to draw a histogram

server <- function(input, output) {
    #Subset data based on user selection
    #filteredData <- reactive({
        #data[data$Species == input$Species]
    #})
    filteredData <- reactive({
        data %>%
        filter(Species == input$Species)})
    #Define colors
    pal <- colorNumeric(
    palette = c('blue', 'gold', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$Predicted)

    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(filteredData()) %>% 
            addTiles() %>%
            addCircles( weight = 1, radius = ~sqrt(Predicted)*250, 
                        popup = ~as.character(Predicted),
                        label = ~as.character(paste0("Population Density: ",
                                 sep = " ", Predicted)),color = ~pal(Predicted),
                                              fillOpacity = 0.5)
            
            })
    
    #observe({
        #define the color pallate for density
        #pal <- colorNumeric(
            #palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
            #domain = filteredData$Predicted)
        #leafletProxy("mymap", data = filteredData())%>%
            #clearShapes() %>%
            #addCircles( weight = 1, radius = ~sqrt(Predicted)*250, popup = ~as.character(Predicted), 
                       #label = ~as.character(paste0("Population Density: ", sep = " ", Predicted), 
                       #color = ~pal(Predicted), fillOpacity = 0.5)
            
        
   # })
            
    
}

# Run the application 
shinyApp(ui = ui, server = server)
