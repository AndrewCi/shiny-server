


library("leaflet")
library("htmltools")
library("shiny")
library("RColorBrewer")
library("dplyr")
library("DT")

route_table <- read.csv('Route_Table.csv',header=TRUE,sep=',',stringsAsFactors=FALSE, check.names = FALSE)
segment_table <- read.csv('Segment_Table.csv',header=TRUE,sep=',',stringsAsFactors=FALSE, check.names = FALSE)

incident_data <- read.csv('Raw_Incident_Data.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)

incident_data$Latitude  <- gsub(",","",incident_data$Latitude)
incident_data$Latitude <- as.numeric(as.character(incident_data$Latitude))

incident_data$Longitude  <- gsub(",","",incident_data$Longitude)
incident_data$Longitude <- as.numeric(as.character(incident_data$Longitude))

incident_data$Density.Score  <- gsub(",","",incident_data$Density.Score)
incident_data$Density.Score <- as.numeric(as.character(incident_data$Density.Score))

incident_data$DD.Check.Score  <- gsub(",","",incident_data$DD.Check.Score)
incident_data$DD.Check.Score <- as.numeric(as.character(incident_data$DD.Check.Score))

incident_data$Seg.Density.Score  <- gsub(",","",incident_data$Seg.Density.Score)
incident_data$Seg.Density.Score <- as.numeric(as.character(incident_data$Seg.Density.Score))

incident_data$Seg.DD.Check.Score  <- gsub(",","",incident_data$Seg.DD.Check.Score)
incident_data$Seg.DD.Check.Score <- as.numeric(as.character(incident_data$Seg.DD.Check.Score))

incident_data_r2011 <- subset(incident_data, DD.Top20Rt == "Yes")

server <- function(input, output, session) {
 
  
  filteredData <- reactive({
    
if(input$dataDisplay == "R20"){
    
  incident_data_new <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange[2]) & (incident_data$Type %in% input$typeSelection) & (incident_data$Crash.Severity %in% input$severitySelection) & (incident_data$Density.Score >= input$metricRange1[1] & incident_data$Density.Score <= input$metricRange1[2]) &(incident_data$Den.Top20Rt == "Yes"),]

}else if(input$dataDisplay == "RS20"){
  incident_data_new <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange[2]) & (incident_data$Type %in% input$typeSelection) & (incident_data$Crash.Severity %in% input$severitySelection) &(incident_data$Den.Top20Rt == "Yes"),]

}else if(input$dataDisplay == "S50"){
  
  incident_data_new <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange[2]) & (incident_data$Type %in% input$typeSelection) & (incident_data$Crash.Severity %in% input$severitySelection) &(incident_data$Den.Top50Seg == "Yes"),]
  
}else if(input$dataDisplay == "RA"){
  
  incident_data_new <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange[2]) & (incident_data$Type %in% input$typeSelection) & (incident_data$Crash.Severity %in% input$severitySelection) & (incident_data$Density.Score >= input$metricRange2[1] & incident_data$Density.Score <= input$metricRange2[2]) ,]
  
}else if(input$dataDisplay == "RSA"){
  
  incident_data_new <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange[2]) & (incident_data$Type %in% input$typeSelection) & (incident_data$Crash.Severity %in% input$severitySelection),]
  
}
  
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
    
    if(input$dataDisplay == "R20"){

    colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current2$Density.Score)
    
    }else if(input$dataDisplay == "RS20"){

      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current2$Seg.Density.Score)
      
    }else if(input$dataDisplay == "S50"){
      
      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current2$Seg.Density.Score)
      
      
    }else if(input$dataDisplay == "RA"){
      
      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current2$Density.Score)
      
      
    }else if(input$dataDisplay == "RSA"){
      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current2$Seg.Density.Score)
    }

  })
  
  colorpal_legend <- reactive({
    data_current5 <- filteredData()

    if(input$dataDisplay == "R20"){

    colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current5$Density.Score)}else if(input$dataDisplay == "RS20"){

      colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current5$Seg.Density.Score)


    }else if(input$dataDisplay == "S50"){
      
      colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current5$Seg.Density.Score)
      
    }else if(input$dataDisplay == "RA"){
      
      colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current5$Density.Score)
      
    }else if(input$dataDisplay == "RSA"){
      
      colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current5$Seg.Density.Score)
      
    }

  })
  
  
  observe({
    
    data_current <- filteredData()

    if(input$dataDisplay == "R20"){

    pal <- colorpal()
    if(is.na(data_current[1,]) || (length(data_current) == 0) || is.na(data_current[length(data_current),])){
    leafletProxy("map", data = data_current) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current$Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>")) 
    }else{
      leafletProxy("map", data = data_current) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current$Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>"))
    }
    }else if(input$dataDisplay == "RS20"){

      if(is.na(data_current[1,]) || (length(data_current) == 0) || is.na(data_current[length(data_current),])){
        leafletProxy("map", data = data_current) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current$Seg.Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>")) 
      }else{
        leafletProxy("map", data = data_current) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current$Seg.Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>"))
      }
      
      
    }else if(input$dataDisplay == "S50"){
      
      if(is.na(data_current[1,]) || (length(data_current) == 0) || is.na(data_current[length(data_current),])){
        leafletProxy("map", data = data_current) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current$Seg.Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>")) 
      }else{
        leafletProxy("map", data = data_current) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current$Seg.Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>"))
      }
      
    }else if(input$dataDisplay == "RA"){
      
      pal <- colorpal()
      if(is.na(data_current[1,]) || (length(data_current) == 0) || is.na(data_current[length(data_current),])){
        leafletProxy("map", data = data_current) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current$Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>")) 
      }else{
        leafletProxy("map", data = data_current) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current$Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>"))
      }
      
      
    }else if(input$dataDisplay == "RSA"){
      
      if(is.na(data_current[1,]) || (length(data_current) == 0) || is.na(data_current[length(data_current),])){
        leafletProxy("map", data = data_current) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current$Seg.Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>")) 
      }else{
        leafletProxy("map", data = data_current) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current$Seg.Density.Score), popup = paste("<br><b>Date:</b> ", data_current$Incident.Date, "</br><br><b>Type:</b> ", data_current$Type, "</br><br><b>Road Name:</b> ", data_current$Road.Name,"</br>"))
      }
      
    }
    
  })

  observe({
    data_current3 <- filteredData()

    if(input$dataDisplay == "R20"){
  
    if(is.na(data_current3[1,]) || (length(data_current3) == 0) || is.na(data_current3[length(data_current3),])){
     leafletProxy("map", data = data_current3) %>% clearControls() %>% removeControl("legend1")

    }else{
      proxy <- leafletProxy("map", data = data_current3)

      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()

      pal <-   colorpal_legend()
      proxy %>% addLegend('bottomright', pal = pal, values = ~data_current3$Density.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
    }

    }else if(input$dataDisplay == "RS20"){

      if(is.na(data_current3[1,]) || (length(data_current3) == 0) || is.na(data_current3[length(data_current3),])){
        leafletProxy("map", data = data_current3) %>% clearControls() %>% removeControl("legend1")

      }else{
        proxy <- leafletProxy("map", data = data_current3)

        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()

        pal <-   colorpal_legend()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current3$Seg.Density.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }

    }else if(input$dataDisplay == "S50"){
      
      if(is.na(data_current3[1,]) || (length(data_current3) == 0) || is.na(data_current3[length(data_current3),])){
        leafletProxy("map", data = data_current3) %>% clearControls() %>% removeControl("legend1")
        
      }else{
        proxy <- leafletProxy("map", data = data_current3)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        
        pal <-   colorpal_legend()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current3$Seg.Density.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }
      
    }else if(input$dataDisplay == "RA"){
      
      if(is.na(data_current3[1,]) || (length(data_current3) == 0) || is.na(data_current3[length(data_current3),])){
        leafletProxy("map", data = data_current3) %>% clearControls() %>% removeControl("legend1")
        
      }else{
        proxy <- leafletProxy("map", data = data_current3)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        
        pal <-   colorpal_legend()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current3$Density.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }
      
    }else if(input$dataDisplay == "RSA"){
      
      if(is.na(data_current3[1,]) || (length(data_current3) == 0) || is.na(data_current3[length(data_current3),])){
        leafletProxy("map", data = data_current3) %>% clearControls() %>% removeControl("legend1")
        
      }else{
        proxy <- leafletProxy("map", data = data_current3)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        
        pal <-   colorpal_legend()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current3$Seg.Density.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }
      
    }

  })
  
  output$map <- renderLeaflet({
 
    leaflet() %>% addProviderTiles(providers$Hydda) %>%  fitBounds(lng1 = max(incident_data$Longitude),lat1 = max(incident_data$Latitude), lng2 = min(incident_data$Longitude),lat2 = min(incident_data$Latitude)) %>% mapOptions(zoomToLimits = "first")
    
  })
  


  filteredData1 <- reactive({
    
    if(input$dataDisplay1 == "R20"){
      
      incident_data_new1 <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange1[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange1[2]) & (incident_data$Type %in% input$typeSelection1) & (incident_data$Crash.Severity %in% input$severitySelection1) & (incident_data$DD.Check.Score >= input$metricRange11[1] & incident_data$DD.Check.Score <= input$metricRange11[2]) &(incident_data$DD.Top20Rt == "Yes"),]
      
    }else if(input$dataDisplay1 == "RS20"){
      incident_data_new1 <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange1[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange1[2]) & (incident_data$Type %in% input$typeSelection1) & (incident_data$Crash.Severity %in% input$severitySelection1) &(incident_data$DD.Top20Rt == "Yes"),]
      
    }else if(input$dataDisplay1 == "S50"){
      
      incident_data_new1 <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange1[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange1[2]) & (incident_data$Type %in% input$typeSelection1) & (incident_data$Crash.Severity %in% input$severitySelection1) &(incident_data$DD.Top50Seg == "Yes"),]
      
    }else if(input$dataDisplay1 == "RA"){
      
      incident_data_new1 <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange1[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange1[2]) & (incident_data$Type %in% input$typeSelection1) & (incident_data$Crash.Severity %in% input$severitySelection1) & (incident_data$DD.Check.Score >= input$metricRange21[1] & incident_data$DD.Check.Score <= input$metricRange21[2]) ,]
      
    }else if(input$dataDisplay1 == "RSA"){
      
      incident_data_new1 <- incident_data[(as.Date(incident_data$Incident.Date,"%m/%d/%Y") >= input$dateRange1[1] & as.Date(incident_data$Incident.Date,"%m/%d/%Y") <= input$dateRange1[2]) & (incident_data$Type %in% input$typeSelection1) & (incident_data$Crash.Severity %in% input$severitySelection1),]
      
    }
    
    return(incident_data_new1)
  })
  
  
  colorpal1 <- reactive({
    data_current21 <- filteredData1()
    
    if(input$dataDisplay1 == "R20"){
      
      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current21$DD.Check.Score)
      
    }else if(input$dataDisplay1 == "RS20"){
      
      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current21$Seg.DD.Check.Score)
      
    }else if(input$dataDisplay1 == "S50"){
      
      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current21$Seg.DD.Check.Score)
      
      
    }else if(input$dataDisplay1 == "RA"){
      
      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current21$DD.Check.Score)
      
      
    }else if(input$dataDisplay1 == "RSA"){
      colorNumeric(palette = c("#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"), domain = data_current21$Seg.DD.Check.Score)
    }
    
  })
  
  colorpal_legend1 <- reactive({
    data_current51 <- filteredData1()
    
    if(input$dataDisplay1 == "R20"){
      
      colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current51$DD.Check.Score)}else if(input$dataDisplay1 == "RS20"){
        
        colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current51$Seg.DD.Check.Score)
        
        
      }else if(input$dataDisplay1 == "S50"){
        
        colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current51$Seg.DD.Check.Score)
        
      }else if(input$dataDisplay1 == "RA"){
        
        colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current51$DD.Check.Score)
        
      }else if(input$dataDisplay1 == "RSA"){
        
        colorNumeric(palette = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee0d2"), domain = data_current51$Seg.DD.Check.Score)
        
      }
    
  })
  
  
  observe({
    
    data_current1 <- filteredData1()
    
    if(input$dataDisplay1 == "R20"){
      
      pal <- colorpal1()
      if(is.na(data_current1[1,]) || (length(data_current1) == 0) || is.na(data_current1[length(data_current1),])){
        leafletProxy("map1", data = data_current1) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current1$DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>")) 
      }else{
        leafletProxy("map1", data = data_current1) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current1$DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>"))
      }
    }else if(input$dataDisplay1 == "RS20"){
      
      if(is.na(data_current1[1,]) || (length(data_current1) == 0) || is.na(data_current1[length(data_current1),])){
        leafletProxy("map1", data = data_current1) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current1$Seg.DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>")) 
      }else{
        leafletProxy("map1", data = data_current1) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current1$Seg.DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>"))
      }
      
      
    }else if(input$dataDisplay1 == "S50"){
      
      if(is.na(data_current1[1,]) || (length(data_current1) == 0) || is.na(data_current1[length(data_current1),])){
        leafletProxy("map1", data = data_current1) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current1$Seg.DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>")) 
      }else{
        leafletProxy("map1", data = data_current1) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current1$Seg.DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>"))
      }
      
    }else if(input$dataDisplay1 == "RA"){
      
      pal <- colorpal1()
      if(is.na(data_current1[1,]) || (length(data_current1) == 0) || is.na(data_current1[length(data_current1),])){
        leafletProxy("map1", data = data_current1) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current1$DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>")) 
      }else{
        leafletProxy("map1", data = data_current1) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current1$DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>"))
      }
      
      
    }else if(input$dataDisplay1 == "RSA"){
      
      if(is.na(data_current1[1,]) || (length(data_current1) == 0) || is.na(data_current1[length(data_current1),])){
        leafletProxy("map1", data = data_current1) %>% clearControls() %>% clearMarkers() %>% removeControl("legend1") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal(data_current1$Seg.DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>")) 
      }else{
        leafletProxy("map1", data = data_current1) %>% clearMarkers() %>% addCircleMarkers(radius = 3, weight = 1, opacity = 1, fill = TRUE, fillOpacity = 1, color = ~pal(data_current1$Seg.DD.Check.Score), popup = paste("<br><b>Date:</b> ", data_current1$Incident.Date, "</br><br><b>Type:</b> ", data_current1$Type, "</br><br><b>Road Name:</b> ", data_current1$Road.Name,"</br>"))
      }
      
    }
    
  })
  
  observe({
    data_current31 <- filteredData1()
    
    if(input$dataDisplay1 == "R20"){
      
      if(is.na(data_current31[1,]) || (length(data_current31) == 0) || is.na(data_current31[length(data_current31),])){
        leafletProxy("map1", data = data_current31) %>% clearControls() %>% removeControl("legend1")
        
      }else{
        proxy <- leafletProxy("map1", data = data_current31)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        
        pal <-   colorpal_legend1()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current31$DD.Check.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }
      
    }else if(input$dataDisplay1 == "RS20"){
      
      if(is.na(data_current31[1,]) || (length(data_current31) == 0) || is.na(data_current31[length(data_current31),])){
        leafletProxy("map1", data = data_current31) %>% clearControls() %>% removeControl("legend1")
        
      }else{
        proxy <- leafletProxy("map1", data = data_current31)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        
        pal <-   colorpal_legend1()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current31$Seg.DD.Check.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }
      
    }else if(input$dataDisplay1 == "S50"){
      
      if(is.na(data_current31[1,]) || (length(data_current31) == 0) || is.na(data_current31[length(data_current31),])){
        leafletProxy("map1", data = data_current31) %>% clearControls() %>% removeControl("legend1")
        
      }else{
        proxy <- leafletProxy("map1", data = data_current31)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        
        pal <-   colorpal_legend1()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current31$Seg.DD.Check.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }
      
    }else if(input$dataDisplay1 == "RA"){
      
      if(is.na(data_current31[1,]) || (length(data_current31) == 0) || is.na(data_current31[length(data_current31),])){
        leafletProxy("map1", data = data_current31) %>% clearControls() %>% removeControl("legend1")
        
      }else{
        proxy <- leafletProxy("map1", data = data_current31)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        
        pal <-   colorpal_legend1()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current31$DD.Check.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }
      
    }else if(input$dataDisplay1 == "RSA"){
      
      if(is.na(data_current31[1,]) || (length(data_current31) == 0) || is.na(data_current31[length(data_current31),])){
        leafletProxy("map1", data = data_current31) %>% clearControls() %>% removeControl("legend1")
        
      }else{
        proxy <- leafletProxy("map1", data = data_current31)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        
        pal <-   colorpal_legend1()
        proxy %>% addLegend('bottomright', pal = pal, values = ~data_current31$Seg.DD.Check.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")
      }
      
    }
    
  })
  
  x <- 0
  
  output$map1<- renderLeaflet({
    
 

  
  
    if((x == 0) & (input$dataDisplay1 == "R20")){
      
      x <- (x + 1)
      
      pal1 <- colorpal1()
      pal2 <- colorpal_legend1()
      
    leaflet(incident_data_r2011) %>% addProviderTiles(providers$Hydda) %>%  fitBounds(lng1 = max(incident_data$Longitude),lat1 = max(incident_data$Latitude), lng2 = min(incident_data$Longitude),lat2 = min(incident_data$Latitude)) %>% mapOptions(zoomToLimits = "first") %>% addCircleMarkers(radius = 1, weight = 5, opacity = 1, fill = FALSE, color = ~pal1(incident_data_r2011$DD.Check.Score
), popup = paste("<br><b>Date:</b> ", incident_data_r2011$Incident.Date, "</br><br><b>Type:</b> ", incident_data_r2011$Type, "</br><br><b>Road Name:</b> ", incident_data_r2011$Road.Name,"</br>")) %>% addLegend('bottomright', pal = pal2, values = ~incident_data_r2011$DD.Check.Score,title = "DUI Density",opacity = 1, labFormat = myLabelFormat(reverse_order = T),layerId = "legend1")}else{
      
      leaflet() %>% addProviderTiles(providers$Hydda) %>%  fitBounds(lng1 = max(incident_data$Longitude),lat1 = max(incident_data$Latitude), lng2 = min(incident_data$Longitude),lat2 = min(incident_data$Latitude)) %>% mapOptions(zoomToLimits = "first") 
      
      
    }
    
    
    
    
  })
  
  
  output$routeTable <- DT::renderDataTable(
    route_table, extensions = c('Buttons','ColReorder'),options = list(
      pageLength = 12, dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), colReorder = TRUE, scrollX=TRUE),rownames = FALSE, caption = "Route Table: Click on Row to Filter Segment Table", class = 'cell-border stripe'
    
    
    )
  
  
  output$rawTable <- DT::renderDataTable(
    incident_data, extensions = c('Buttons','ColReorder'),options = list(
      pageLength = 8, dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), colReorder = TRUE, scrollX=TRUE),rownames = FALSE, caption = "Raw Data of DUI Accidents and Arrrests from 2010 through 2017", class = 'cell-border stripe'
    
    
  )
  
  output$segmentTable <- DT::renderDataTable(
    
    
    
    filteredSegments(), 
    
    
    
    extensions = c('Buttons','ColReorder'),options = list(
      pageLength = 12 ,dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), colReorder = TRUE, scrollX=TRUE),rownames = FALSE,caption = "Segment Table", class = 'cell-border stripe'
    
    
    
    
  )
  
  
  filteredSegments <- reactive({
    
    
if(length(input$routeTable_rows_selected) == 0){
  
  return(segment_table)
  
}else{
    
  routes <- vector("list", length(input$routeTable_rows_selected))


  
    for(i in 1:length(input$routeTable_rows_selected)){
      routes[[i]] <- route_table$Route[input$routeTable_rows_selected[i]]
    }

      segment_table_select <- segment_table[(segment_table$Route %in% routes),]


    return(segment_table_select)
}

  })
  

  
}


