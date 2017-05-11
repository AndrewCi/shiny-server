
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

incident_data_r20 <- subset(incident_data, Den.Top20Rt == "Yes")
incident_data_s50 <- subset(incident_data, Den.Top50Seg == "Yes")

incident_data_r201 <- subset(incident_data, DD.Top20Rt == "Yes")
incident_data_s501 <- subset(incident_data, DD.Top50Seg == "Yes")

ui <- navbarPage(HTML("<b>DUI Incident Visualization</b>"),header = "",windowTitle = "DUI Incident Visualization" ,  navbarMenu("Albemarle County",tabPanel("DUI Density Map", bootstrapPage(

  titlePanel(HTML("<header style=\"margin-top: -20px;\"><b><h4>&nbsp; &nbsp;Map of Albemarle County DUI Incidents by Density Metric</b></h4>"), windowTitle = "Albemarle County DUI"),
 br(),
 


sidebarPanel(

  selectInput("dataDisplay",HTML("<b><h5>Select Dataset to Display:</b></h5>"),choices = c("Top 20 Routes (by route)" = "R20","Top 20 Routes (by segment)" = "RS20","Top 50 Segments" = "S50","All Incidents (by route)" = "RA","All Incidents (by segment)" = "RSA"), selected = c("Top 20 Routes (by route)" = "R20")),
  
  dateRangeInput("dateRange", HTML("<b><h5>Select Time Frame:</b></h5>") , start = min(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), end = max(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), min = min(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), max = Sys.Date(),format = "mm-dd-yyyy"),

  

  checkboxGroupInput("typeSelection", HTML("<b><h5>Select Type of Incident:</b></h5>"), choices = c("Accident" = "Accident", "Arrest" = "Arrest"), selected =c("Accident" = "Accident", "Arrest" = "Arrest")),

 

  checkboxGroupInput("severitySelection", HTML("<b><h5>Select Severity of Incident:</b></h5>"), choices = c("Property Damage" = "property damage crash", "Injury" = "injury crash","Pedestrian Injury" = "pedestrian injury crash","Fatal" = "fatal crash", "Unlabeled" = ""), selected = c("Property Damage" = "property damage crash", "Injury" = "injury crash","Pedestrian Injury" = "pedestrian injury crash","Fatal" = "fatal crash", "Unlabeled" = "")),



  
  
  
  
  conditionalPanel(
    
    condition = "input.dataDisplay == 'R20'",
    sliderInput("metricRange1",HTML("<b><h5>Select Density Metric Range:</b></h5>"), min = 0, max = round(max(incident_data_r20$Density.Score)+.005,3), value = as.numeric(c(0,.12)), step = .004, dragRange = FALSE)
  ),
  conditionalPanel(
    
    condition = "input.dataDisplay == 'RS20'"
   # sliderInput("metricRange2",HTML("<b><h5>Select Density Metric Range:</b></h5>"), min = 0, max = round(max(incident_data_r20$Seg.Density.Score)+.005,3), value = as.numeric(c(0,round(max(incident_data_r20$Seg.Density.Score),3))), step = .004, dragRange = FALSE)
    
  ),
  
  conditionalPanel(
    
    condition = "input.dataDisplay == 'S50'"
  
  
  ),
  
  conditionalPanel(
    
    condition = "input.dataDisplay == 'RA'",
    
    sliderInput("metricRange2",HTML("<b><h5>Select Density Metric Range:</b></h5>"), min = 0, max = round(max(incident_data$Density.Score)+.005,3), value = as.numeric(c(0,.12)), step = .004, dragRange = FALSE)
  ),
  
  conditionalPanel(
    
    condition = "input.dataDisplay == 'RSA'"
    

    
    
  )),
 





mainPanel(leafletOutput("map", height = 575))
)),
tabPanel("DUI Estimation Map",
         
         bootstrapPage(
           
           
           
titlePanel(HTML("<header style=\"margin-top: -20px;\"><b><h4>&nbsp; &nbsp;Map of Albemarle County DUI Incidents by Estimation Metric</b></h4>"), windowTitle = "Albemarle County DUI"),
br(),
           
sidebarPanel(
             
selectInput("dataDisplay1",HTML("<b><h5>Select Dataset to Display:</b></h5>"),choices = c("Top 20 Routes (by route)" = "R20","Top 20 Routes (by segment)" = "RS20","Top 50 Segments" = "S50","All Incidents (by route)" = "RA","All Incidents (by segment)" = "RSA"), selected = c("Top 20 Routes (by route)" = "R20")),
             
dateRangeInput("dateRange1", HTML("<b><h5>Select Time Frame:</b></h5>") , start = min(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), end = max(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), min = min(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), max = Sys.Date(),format = "mm-dd-yyyy"),
             
             
             
checkboxGroupInput("typeSelection1", HTML("<b><h5>Select Type of Incident:</b></h5>"), choices = c("Accident" = "Accident", "Arrest" = "Arrest"), selected =c("Accident" = "Accident", "Arrest" = "Arrest")),
             
             
             
checkboxGroupInput("severitySelection1", HTML("<b><h5>Select Severity of Incident:</b></h5>"), choices = c("Property Damage" = "property damage crash", "Injury" = "injury crash","Pedestrian Injury" = "pedestrian injury crash","Fatal" = "fatal crash", "Unlabeled" = ""), selected = c("Property Damage" = "property damage crash", "Injury" = "injury crash","Pedestrian Injury" = "pedestrian injury crash","Fatal" = "fatal crash", "Unlabeled" = "")),
             

             
conditionalPanel(
               
condition = "input.dataDisplay1 == 'R20'",
sliderInput("metricRange11",HTML("<b><h5>Select Density Metric Range:</b></h5>"), min = 0, max = round(max(incident_data_r201$DD.Check.Score)+.005,3), value = as.numeric(c(0,2.7)), step = .1, dragRange = FALSE)
),
conditionalPanel(
               
condition = "input.dataDisplay1 == 'RS20'"
               # sliderInput("metricRange2",HTML("<b><h5>Select Density Metric Range:</b></h5>"), min = 0, max = round(max(incident_data_r20$Seg.Density.Score)+.005,3), value = as.numeric(c(0,round(max(incident_data_r20$Seg.Density.Score),3))), step = .004, dragRange = FALSE)
               
),
             
conditionalPanel(
               
condition = "input.dataDisplay1 == 'S50'"
               
               
),
             
conditionalPanel(
               
condition = "input.dataDisplay1 == 'RA'",
               
sliderInput("metricRange21",HTML("<b><h5>Select Density Metric Range:</b></h5>"), min = 0, max = round(max(incident_data$DD.Check.Score)+.005,3), value = as.numeric(c(0,2.7)), step = .1, dragRange = FALSE)),
             
conditionalPanel(
               
condition = "input.dataDisplay1 == 'RSA'"
               
               
               
               
             )),
           
           
           
           
           
mainPanel(leafletOutput("map1", height = 575))
         )
         
         
         
         
         
         
         
         
         
),tabPanel("Route/Segment Data Explorer", 
           
           fluidPage(
          
             
             titlePanel(HTML("<header style=\"margin-top: -20px;\"><b><h4>Route/Segment Data Explorer</b></h4>")), windowTitle = "DUI Data Explorer",
             br(),
             
             
             
             fluidRow(
               
               column(6, DT::dataTableOutput("routeTable")),
               
               
               
               column(6, DT::dataTableOutput("segmentTable"))
             )
           
           
           
           
           
           )),tabPanel("Raw Data Explorer",
                       
                       
                       
                       fluidPage(
                         
                         
                         titlePanel(HTML("<header style=\"margin-top: -20px;\"><b><h4>Route/Segment Data Explorer</b></h4>")), windowTitle = "DUI Data Explorer",
                         br(),
                         
                         
                         
                         fluidRow(
                           
                           column(12, DT::dataTableOutput("rawTable"))
                           
                           
                           
                           
                         )
                         
                         
                         
                         
                         
                       )
                       
                       
                       
                       
                       )
),navbarMenu("Richmond")


)
   #mainPanel(tableOutput("table"))



