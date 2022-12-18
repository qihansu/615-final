library(shiny)
library(shinythemes)
library(readr)
library(rjson)
library(leaflet)
library(MAP)
library(mapproj)
library(maps)
library(ggplot2)
library(rgdal)
library(sp)
library(sf)
library(gridExtra)
library(data.table)
library(DT)
library(dplyr)
library(magrittr)





data_subway <- read.csv("subway.csv")




basemap<-leaflet() %>%
  addTiles() %>% 
  setView(lng = mean(data_subway$from_lon,na.rm=T), lat = mean(data_subway$from_lat,na.rm=T), zoom = 13)
basemap

ui <- fluidPage(
  titlePanel("MBTA Boston"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(selectInput(
        inputId = 'fromstop_name',
        label = 'From',
        choices = c("Park Street", "South Station", "Airport"),
        selected = "Park Street")),
      br(),
      fluidRow(selectInput(
        inputId ='tostop_name ',
        label = 'To',
        choices = c("Harvard", "Broadway", "Government Center"),
        selected = "Harvard")),
      br(),
      fluidRow(selectInput(
        inputId = "service_date",
        label = "Select One Day",
        choices = unique(data_subway$service_date),
        selected = "2021-10-25"))),
    
    mainPanel(
      titlePanel("Subway Map & Information Table"),
      fluidRow("Map", leafletOutput("map", 
                                    width=800,height=500),
               "Table",
               DT::dataTableOutput("table")),  
    
  ))) 
                    
                      
                          
  


                
                
              


                 
                 
                 

                 
server <- function(input, output, session){

  output$map <- renderLeaflet({
                    leaflet() %>%
                    addTiles() %>% 
                   setView(lng = -71.04, lat = 42.35, zoom = 13)%>%
      
      addMarkers(basemap, lat = 42.3564, lng = -71.0624, popup= "The mean travel time is 614.9521", label = 'Park st') %>%
      addMarkers(basemap, lat = 42.3734, lng = -71.1189, popup= "The mean travel time is 614,9521", label = 'Harvard') %>%
      addMarkers(basemap, lat = 42.3522, lng = -71.0552, popup= "The mean travel time is 137.8319", label = 'South station') %>%
      addMarkers(basemap, lat = 42.3426, lng = -71.0569, popup= "The mean travel time is 137.8319", label = 'Broadway') %>%
      addMarkers(basemap, lat = 42.3742, lng = -71.0303, popup= "The mean travel time is 432.0175", label = 'Airport') %>%
      addMarkers(basemap, lat = 42.3597, lng = -71.0592, popup= "The mean travel time is 432.0175", label = 'Government center')%>%
      addPolylines(data = filter(data_subway, fromstop_name =="Park Street"), 
                   lng = ~ c(from_lon, to_lon), 
                   lat = ~ c(from_lat, to_lat), 
                   label = "The mean travel time is 614.9521 sec.",
                   color = "purple")%>%
      addPolylines(data = filter(data_subway, fromstop_name =="South Station"), 
                   lng = ~ c(from_lon, to_lon), 
                   lat = ~ c(from_lat, to_lat), 
                   label = "The mean travel time is 137.8319 sec.",
                   color = "red")%>%
     addPolylines(data = filter(data_subway, fromstop_name =="Airport"), 
                 lng = ~ c(from_lon, to_lon), 
                 lat = ~ c(from_lat, to_lat), 
                 label = "The mean travel time is 432.0175 sec.",
                 color = "blue")
    
  })
  
   

  output$table <- DT::renderDataTable({
    data_subway1 <- data_subway[,c(1,4,6:8,13:15)] %>% filter(fromstop_name==input$fromstop_name) %>%
      filter(service_date == input$service_date)
      
    
  })
    

    

   
    }
 
                 
                 
shinyApp(ui = ui, server = server)
                 



