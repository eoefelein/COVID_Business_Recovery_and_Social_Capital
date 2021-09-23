library(sf)
library(tigris)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

# data loading and processing
USA <- st_read(dsn = 'data/cb_2018_us_county_5m.shp')
counties_sf <- st_as_sf(USA)
counties_reproject_sf <- st_transform(counties_sf, 4326) %>% filter(COUNTYFP < 60010)

emp_rate <- read_csv('data/synthetic_emp_rate_pred.csv')
emp_rate$countyfips <- sprintf("%05d", emp_rate$countyfips)
states_sf_coef <- geo_join(counties_reproject_sf, emp_rate, "GEOID", "countyfips", how='inner')

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title="Employee Rate Data"),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          "Maps",
          tabName = "maps",
          icon=icon("globe")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "maps",
          tags$style(type="text/css","#all_airports {height:calc(100vh - 80px) !important;}"),
          fluidRow(column(4),
                   column(8,
                          selectInput(inputId = "FromCounty",
                                      label="from",
                                      choices=c(unique(emp_rate$countyname)),
                                      selected = 'Travis County, Texas'
                          ),
                          selectInput(inputId = "ToCounty",
                                      label = "to",
                                      choices=c(unique(emp_rate$countyname))
                          ))),
          actionButton("zoomer","go"),
          leafletOutput("map")
          
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # map
  output$map <- renderLeaflet({
    mypal <- colorNumeric(palette="viridis", domain=states_sf_coef$rand_pred, na.color="transparent")
    # mypalette(c(45,43))
    
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lat = 38.2393,
              lng = -96.3795,
              zoom = 4) %>%
      addPolygons(
        data = states_sf_coef,
        fillColor = ~mypal(rand_pred),
        # fillColor = ~ mypal(data$value),
        stroke = FALSE,
        smoothFactor = 0.2,
        fillOpacity = 0.3,
        popup = paste(
          "Region: ",
          states_sf_coef$countyname,
          "<br>",
          "Social Index: ",
          states_sf_coef$rand_pred,
          "<br>"
        )
      )  
    # %>%
      # addLayersControl(
      #   baseGroups = c("Employment Prediction Data (default)", "To-From"),
      #   options = layersControlOptions(collapsed = FALSE)
      # )
  })
  
  map_proxy <- leafletProxy("map")
  
  observeEvent(input$zoomer, { # add Smith, county, kansas and default to zoom = 1?
    # fromCounty
    fromCountyInput <- reactive({
      states_sf_coef %>% dplyr::filter(countyname == input$FromCounty)
    })
    fromData <- fromCountyInput()
    fromCoords <- st_coordinates(st_centroid(fromData$geometry))
    
    # toCounty
    toCountyInput <- reactive({
      states_sf_coef %>% dplyr::filter(countyname == input$ToCounty)
    })
    toData <- toCountyInput()
    toCoords <- st_coordinates(st_centroid(toData$geometry))
    
    
    map_proxy %>%
      flyTo(lng = fromCoords[1], lat = fromCoords[2], zoom = 10)
      flyTo(lng = toCoords[1], lat = toCoords[2], zoom = 10)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)