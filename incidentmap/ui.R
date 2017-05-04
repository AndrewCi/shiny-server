
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




ui <- navbarPage(HTML("<b>DUI Incident Visualization</b>"),windowTitle = "DUI Incident Visualization" ,tabPanel("Albemarle County Density" , bootstrapPage(



 titlePanel(HTML("<b><h4>Map of Albemarle County DUI Accidents and Arrests</b></h4>"), windowTitle = "Albemarle County DUI"),
br(),

sidebarPanel(

  dateRangeInput("dateRange", HTML("<b><h5>Select Time Frame:</b></h5>") , start = min(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), end = max(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), min = min(as.Date(incident_data$Incident.Date,"%m/%d/%Y")), max = Sys.Date(),format = "mm-dd-yyyy"),

  br(),

  checkboxGroupInput("typeSelection", HTML("<b><h5>Select Type of Incident:</b></h5>"), choices = c("Accident" = "Accident", "Arrest" = "Arrest"), selected =c("Accident" = "Accident", "Arrest" = "Arrest")),

  br(),

  checkboxGroupInput("severitySelection", HTML("<b><h5>Select Severity of Incident:</b></h5>"), choices = c("Property Damage" = "property damage crash", "Injury" = "injury crash","Pedestrian Injury" = "pedestrian injury crash","Fatal" = "fatal crash", "Unlabeled" = ""), selected = c("Property Damage" = "property damage crash", "Injury" = "injury crash","Pedestrian Injury" = "pedestrian injury crash","Fatal" = "fatal crash", "Unlabeled" = "")),

  br(),

  sliderInput("metricRange",HTML("<b><h5>Select Density Metric Range:</b></h5>"), min = 0, max = round(max(incident_data$Score)+.005,3), value = as.numeric(c(0,round(max(incident_data$Score)+.005,3))), step = .002, dragRange = FALSE)),





mainPanel(leafletOutput("map", height = 575))
)
),tabPanel("Testing 123")
)
   #mainPanel(tableOutput("table"))



