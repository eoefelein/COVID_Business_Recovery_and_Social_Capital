library(sf)
library(tigris)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(RColorBrewer)

# data loading and processing
USA <- st_read(dsn = '~/Downloads/cb_2018_us_county_5m.shp')
counties_sf <- st_as_sf(USA)
counties_reproject_sf <-
  st_transform(counties_sf, 4326) %>% filter(COUNTYFP < 60010)

emp_rate <-
  read_csv('../socialCapitalEmployment_final_project_files/emp_rate_pred.csv')
emp_rate$countyfips <- sprintf("%05d", emp_rate$countyfips)
emp_rate[, 'oct1_pred'] <- (round(emp_rate[, 'oct1_pred'], 4))
emp_rate <- emp_rate %>%
  unite("label",
        countyname:oct1_pred,
        sep = "\n",
        remove = FALSE)
emp_rate$row_num <- seq(744)
states_sf_coef <-
  geo_join(counties_reproject_sf, emp_rate, "GEOID", "countyfips", how =
             'inner')
states_sf_coef$coords <- st_centroid(states_sf_coef[, 'geometry'])
xx <- unlist(states_sf_coef$coords)
longitude <- xx[seq(1, length(xx), 2)]
latitude <- xx[-seq(1, length(xx), 2)]
# states_sf_coef %>%
#   mutate(lat = unlist(map(states_sf_coef$coords,1)),
#          lng = unlist(map(states_sf_coef$coords,2)))
states_sf_coef$lat <- latitude
states_sf_coef$lon <- longitude

ui <-
  bootstrapPage(
    h1(("Employee Rate Data"), align = "center", class = "header shadow-dark"),
    column(1, ),
    div(
      tags$style(type = "text/css", "#all_airports {height:calc(100vh - 80px) !important;}"),
      div(
        style = "display:inline-block",
        selectInput(
          inputId = "FromCounty",
          label = "from",
          choices = c(unique(emp_rate$countyname)),
          selected = 'Travis County, Texas'
        )
      ),
      div(
        style = "display:inline-block",
        selectInput(
          inputId = "ToCounty",
          label = "to",
          choices = c(unique(emp_rate$countyname)),
          selected = 'San Francisco County, California'
        )
      )
    ),
    column(9, ),
    fluidRow(actionButton("zoomer", "Make your Move")),
    div(leafletOutput("map")),
    verbatimTextOutput("text", placeholder = TRUE)
  )

server <- function(input, output, session) {
  # map
  output$map <- renderLeaflet({
    rwb <- brewer.pal(n = 3, name = "RdBu")
    binpal <- colorQuantile(rwb, states_sf_coef$oct1_pred, n = 3)
    
    leaflet(data = states_sf_coef) %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      setView(lat = 38.2393,
              lng = -96.3795,
              zoom = 4) %>%
      addPolygons(
        data = states_sf_coef,
        color = ~ binpal(oct1_pred),
        stroke = FALSE,
        smoothFactor = 0.2,
        fillOpacity = 0.8
      ) %>%
      addControl(html = actionButton("reset", "Reset", icon = icon("arrows-alt")),
                 position = "topright") %>% 
      addLegend(position = "bottomleft",
                pal = binpal, 
                values = emp_rate$oct1_pred)
      })
  
  map_proxy <- leafletProxy("map")
  
  # Show a popup at the given location
  show_popup_on_mouseover <- function(id, lat, lng) {
    selected_point <- emp_rate[row_num == id, ]
    content <- as.character(selected_point$label)
    map_proxy %>%
      addPopups(lon, lat, content)
  }
  
  observeEvent(input$mymap_shape_mouseout$id, {
    map_proxy %>% clearPopups()
  })
  
  # When circle is hovered over...show a popup
  observeEvent(input$mymap_shape_mouseover$id, {
    pointId <- input$mymap_shape_mouseover$id
    lat = emp_rate[emp_rate$row_num == pointId, lat]
    lng = emp_rate[emp_rate$row_num == pointId, lon]
    
    map_proxy %>% addPopups(lat = lat, lng = lng, as.character(pointId))
  })
  
  observeEvent(input$zoomer, {
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
      setView(lng = fromCoords[1],
              lat = fromCoords[2],
              zoom = 10) %>%
      addPopups(
        fromCoords[1],
        fromCoords[2],
        popup = paste(
          "County: ",
          fromData$countyname,
          "<br>",
          "Employment Rate: ",
          fromData$oct1_pred,
          "<br>"
        )
      ) %>%
      flyTo(
        lng = toCoords[1],
        lat = toCoords[2],
        zoom = 10,
        options = list(duration = 10)
      )  %>%
      addPopups(
        toCoords[1],
        toCoords[2],
        popup = paste(
          "County: ",
          toData$countyname,
          "<br>",
          "Employment Rate: ",
          toData$oct1_pred,
          "<br>"
        )
      )
    output$text <- renderPrint({
      if (toData$oct1_pred > fromData$oct1_pred) {
        cat("Great Move!")
      } else {
        cat("I would stay put if I were you... ")
      }
    })
  })
  
  observeEvent(input$reset, {
    map_proxy %>% setView(lat = 38.2393,
                          lng = -96.3795,
                          zoom = 4)
    output$text <- renderPrint({
      cat("")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
