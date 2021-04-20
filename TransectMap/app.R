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
ui <- fluidPage( shinythemes::themeSelector(),

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
                 "Microcebus_rufus" ), selected = "Avahi_laniger"),
    
    selectInput("site","Select Site", 
                choices = c("Ampatsoana","Maharira","Miaranony",
                            "Valohoaka","Vohiparara","ALL"), selected = "ALL")
    )
    ))
        


# Define server logic required to draw a histogram

server <- function(input, output) {
    #Subset data based on user selection
    
    filteredData <- reactive({
        data %>%
        filter(Species == input$Species)%>%
        filter(if(input$site != 'ALL')(Site == input$site) else TRUE)
                })
    
    #Define colors
    pal <- colorNumeric(
    palette = c('navy','royal blue 1','cyan', 'gold', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$Predicted)

    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(filteredData()) %>% 
            addTiles() %>%
            addCircles( weight = 1, radius = ~sqrt(Predicted)*250, 
                        popup = ~as.character(Predicted),
                        label = ~as.character(paste0("Population Density: ",
                                 sep = " ", Predicted)),color = ~pal(Predicted),
                                              fillOpacity = 0.9)%>%
            addLegend("bottomright",pal = pal, values = ~Predicted,
                      title = "Population Density",
                      opacity=1)
            
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
