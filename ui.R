

library("leaflet")
library("htmltools")
library("shiny")
library("RColorBrewer")
library("dplyr")



ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
 titlePanel("Map of Albemarle County DUI Accidents and Arrests", windowTitle = "Albemarle County DUI"),
br(),
sidebarPanel(
  
  dateRangeInput("dateRange", HTML("<b><h4>Select Time Frame:</b></h4>") , start = min(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), end = max(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), min = min(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), max = Sys.Date(),format = "mm-dd-yyyy"),
  
  br(),
  
  checkboxGroupInput("typeSelection", HTML("<b><h4>Select Type of Incident:</b></h4>"), choices = c("Accident" = "Accident", "Arrest" = "Arrest"), selected =c("Accident" = "Accident", "Arrest" = "Arrest")),
  
  br(), 
  
  checkboxGroupInput("severitySelection", HTML("<b><h4>Select Severity of Incident:</b></h4>"), choices = c("Property Damage" = "property damage crash", "Injury" = "injury crash","Pedestrian Injury" = "pedestrian injury crash","Fatal" = "fatal crash", "Unlabeled" = ""), selected = c("Property Damage" = "property damage crash", "Injury" = "injury crash","Pedestrian Injury" = "pedestrian injury crash","Fatal" = "fatal crash", "Unlabeled" = "")),
  
  br(),
  
  sliderInput("metricRange",HTML("<b><h4>Select Density Metric Range:</b></h4>"), min = 0, max = round(max(incident_data$Score)+.005,3), value = as.numeric(c(0,round(max(incident_data$Score)+.005,3))), step = .002, dragRange = FALSE)),





mainPanel(leafletOutput("map"))
   
   #mainPanel(tableOutput("table"))            
                
                
                
                
                
                
                
                
                )
