

runtime: shiny
library("leaflet")
library("htmltools")
library("shiny")
library("RColorBrewer")
library("dplyr")


incident_data <- read.csv('Incident_Data.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)

incident_data$Latitude  <- gsub(",","",incident_data$Latitude)
incident_data$Latitude <- as.numeric(as.character(incident_data$Latitude))

incident_data$Longitude  <- gsub(",","",incident_data$Longitude)
incident_data$Longitude <- as.numeric(as.character(incident_data$Longitude))

incident_data$Score  <- gsub(",","",incident_data$Score)
incident_data$Score <- as.numeric(as.character(incident_data$Score))

server <- function(input, output, session) {
 
  
  filteredData <- reactive({
    
    # incident_data %>% 
    #   filter(
    #     Incident.Date > input$dateRange[1] & Incident.Date < input$dateRange[2]) %>% 
    #   
    #   filter(
    #     Type %in% input$typeSelection & Crash.Severity %in% input$severitySelection
    #   ) %>% 
    # filter(
    #   Score > input$metricRange[1] & Score < input$metricRange[2]
    # ) %>% as.data.frame()
    # 
    

   #  incident_data <- incident_data[as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange[2],]
   # 
   #  incident_data <- incident_data[incident_data$Type %in% input$typeSelection,]
   # 
   # incident_data <- incident_data[incident_data$Crash.Severity %in% input$severitySelection,]
   # 
   #  incident_data <- incident_data[incident_data$Score >= input$metricRange[1] & incident_data$Score <= input$metricRange[2],]
    
  incident_data_new <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange[2]) & (incident_data$Type %in% input$typeSelection) & (incident_data$Crash.Severity %in% input$severitySelection) & (incident_data$Score >= input$metricRange[1] & incident_data$Score <= input$metricRange[2]),]

    return(incident_data_new)

})
  
  myLabelFormat = function(..., reverse_order = FALSE){ 
    if(reverse_order){ 
      function(type = "numeric", cuts){ 
        cuts <- sort(cuts, decreasing = T)
      } 
    }else{
      labelFormat(...)
    }
  }

  colorpal <- reactive({
    data_current2 <- filteredData()
    colorNumeric(palette = c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current2$Score)

  })
  
  colorpal_legend <- reactive({
    data_current5 <- filteredData()
    colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1"), domain = data_current5$Score)
    
  })

  output$map <- renderLeaflet({
    
  
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
      


      leaflet() %>% addProviderTiles(providers$Hydda)
 
    
   

  })

  observe({
    data_current <- filteredData()
    pal <- colorpal()
    if(is.na(data_current[1,]) || (length(data_current) == 0) || is.na(data_current[length(data_current),])){
    leafletProxy("map", data = data_current) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current$Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>",data_current$Score)) 
    }else{
      leafletProxy("map", data = data_current) %>% clearMarkers() %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current$Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>"))
    }
  })

  observe({
    data_current3 <- filteredData()
   
    if(is.na(data_current3[1,]) || (length(data_current3) == 0) || is.na(data_current3[length(data_current3),])){
     leafletProxy("map", data = data_current3) %>% clearControls() %>% removeControl("legend1")
      
      
    }else{
      proxy <- leafletProxy("map", data = data_current3)
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      
      pal <-   colorpal_legend()
      proxy %>% addLegend('bottomright', pal = pal, values = ~data_current3$Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
    }
   
  })

  
  # output$table <- renderTable({
  #   filteredData()
  # })
  
}


