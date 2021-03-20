library(flexdashboard)
library(readr)
library(shiny)
library(ggplot2)
library(data.table)
library(DT)
library(flexdashboard)
library(dplyr)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(plotly)

# Read in data
data<-read.csv("311data.csv")

## Read in shape of police zone from API
police_zone<-readOGR("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/PGHWebPoliceZones/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=")
data$date<-as.Date(data$date)
data$weekday<-weekdays(data$date)

## Aggregate data for chart visualization
data_agg<-data
setDT(data_agg)
data_agg=data_agg[,c("REQUEST_ORIGIN","weekday")]
data_agg=data_agg[,.N,by=.(weekday,REQUEST_ORIGIN)]


ui <- navbarPage("Pittsburgh Emergency Request Analysis",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Select Sewer Type
                              radioButtons(
                                inputId = "Type",
                                label="Type of information to display in the GIS graph",
                                choices = c("POLICE_ZONE", "REQUEST_ORIGIN", "GEO_ACCURACY"),
                                selected = "REQUEST_ORIGIN"),
                              
                            ),
                            # Map Panel
                            mainPanel(
                              tags$style(type = "text/css", "#leaflet {height: calc(100vh - 80px) !important;}"),
                              # Map Page
                              leafletOutput("leaflet")
                            )
                          )
                 ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            downloadButton('downloadbutton', 'Download The Covid Data Here!'),
                            wellPanel(DT::dataTableOutput("table"))
                          )
                          
                 ),
                 
                 tabPanel("Barplot for Emergency Type",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = "Request_Type",
                                label="Contact approach of the incident",
                                choices = unique(data$REQUEST_ORIGIN),
                                selected = "Website"),
                              
                            ),
                            # Panel for barplot
                            mainPanel(
                              plotlyOutput(outputId = "Barplot")
                            )
                          )
                          
                 ),
                 # Panel for scatterplot
                 tabPanel("Scatterplot for coordinates",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(
                                inputId = "weekday",
                                label="days of week for scatterplot",
                                choices = unique(data$weekday),
                                selected = unique(data$weekday)),
                              
                            ),
                            
                            mainPanel(
                              plotlyOutput(outputId = "Scatterplot")
                            )
                          )
                          
                 )
)
server <- function(input, output) {
  
  ## Download handler 
  output$downloadbutton <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(data, con)
    }
  )
  
  ## Filter data for scatterplot display
  dataInputs <- reactive({
    dataInf <- data 
    dataInf <- subset(dataInf, weekday %in% input$weekday)
    return(dataInf)
  })
  ## Filter data for Request Type to display in Barplot
  dataType <- reactive({
    dataInf <- data_agg 
    dataInf <- subset(dataInf, REQUEST_ORIGIN == input$Request_Type)
    return(dataInf)
  })
  
  ##DataTable output
  output$table<-renderDT(data, options = list(lengthChange = FALSE))

  output$Scatterplot<-renderPlotly({
    data=dataInputs()
    plot_ly(data = data, x = ~X, y = ~Y,color=~data$weekday)
    
  })
  
  ## Barplot output
  output$Barplot<-renderPlotly({
    data_agg=dataType()
    plot_ly(data = data_agg, x =~weekday, y = ~N,type="bar")
    
  })
  
  ##Map output
  output$leaflet <- renderLeaflet({
    ## map setup
    leaflet()%>%
      addProviderTiles("OpenStreetMap.HOT")%>%
      setView(-80,40.4,zoom = 10)
  })
  
  ## Observer for circlemarkers
  observe({
    data=dataInputs()
    data$Selected <- data[[input$Type]]
    pal311 <- colorFactor(topo.colors(length(unique(data$Selected))), unique(data$Selected)) 
    leafletProxy("leaflet", data = data) %>%
      clearMarkers() %>%
      clearControls()%>%
      addCircleMarkers(lng = ~X, lat = ~Y, radius = 1.5, color = ~pal311(Selected), clusterOptions = markerClusterOptions()) %>%
      addLegend(position = "topright" , pal = pal311, values = data$Selected, title = "REQUEST_ORIGIN")
  })
  
  ## Observer for police_zone
  observe({
    data=dataInputs()
    data$Selected <- data[[input$Type]]
    pal <- colorNumeric(
      palette = "Purples",
      domain = police_zone$zone)
    ## Only display Polygon when police_zone is chosen
    if(input$Type=="POLICE_ZONE"){
      leafletProxy("leaflet", data = police_zone) %>%
        clearShapes()%>%
        addPolygons(color="Purple",popup =~paste(zone))
    }else{
      leafletProxy("leaflet", data = police_zone) %>%
        clearShapes()
    }
  })
}

shinyApp(ui = ui, server = server)
