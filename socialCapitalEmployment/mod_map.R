# data loading and processing
USA <- st_read(dsn = 'data/cb_2018_us_county_5m.shp')
counties_sf <- st_as_sf(USA)
counties_reproject_sf <-
  st_transform(counties_sf, 4326) %>% filter(COUNTYFP < 60010)

social_indices <- read_tsv("data/capturing-dataset.tsv")
drops <- c("geoid", 'bridging', 'bonding', 'linking')
social_indices <-
  social_indices[, !(names(social_indices) %in% drops)]
social_indices$fips_n <- sprintf("%05d", social_indices$fips_n)
social_indices <-
  social_indices[, c(
    "fips_n",
    "countyname",
    "socialcap",
    "religion" ,
    "civic",
    "charitable",
    "fraternal",
    "union",
    "voteage",
    "local",
    "state",
    "federal",
    "politicalacts",
    "ethnic",
    "ethnichpn",
    "education",
    "incomeequal",
    "employ",
    "genderincome",
    "language",
    "commu",
    "nonelder"
  )]
social_indices <- pivot_longer(social_indices, cols = 3:22)
names(social_indices)[names(social_indices) == 'name'] <- 'socialIDX'
# merge shape file with data
states_sf_idx <-
  geo_join(counties_reproject_sf, social_indices, "GEOID", "fips_n", how =
             'inner')
print(colnames(states_sf_idx))
cut_borders <- function(x) {
  pattern <-
    "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  start <- as.numeric(gsub(pattern, "\\2", x))
  end <- as.numeric(gsub(pattern, "\\3", x))
  sort(unique(c(start, end)))
}

mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(
    column(10, leafletOutput(ns("map"), height = "100vh")),
    column(
      2,
      shinyWidgets::radioGroupButtons(
        inputId = ns("idx"),
        label = "Metric",
        choices = c(unique(social_indices$socialIDX)),
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    )
  ))
}

mod_map_server <- function(id)  {
  moduleServer(id, function(input, output, session) {
    geoDatasetInput <- reactive({
      states_sf_idx %>% dplyr::filter(socialIDX == input$idx)
      
    })
    
    #Sortie map
    output$map <- renderLeaflet({
      data <- geoDatasetInput()
      bins <- cut(as.vector(data$value), 8, include.lowest = TRUE)
      mypal <-
        colorBin("YlOrRd", domain = data$idx, bins = cut_borders(bins))
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lat = 39.8283,
                lng = -96.5795,
                zoom = 4) %>%
        addPolygons(
          data = data,
          fillColor = ~ mypal(data$value),
          stroke = FALSE,
          smoothFactor = 0.2,
          fillOpacity = 0.3,
          popup = paste(
            "Region: ",
            data$countyname,
            "<br>",
            "Social Index: ",
            data$value,
            "<br>"
          )
        ) %>%
        addLegend(
          position = "bottomleft",
          pal = mypal,
          values = data$idx,
          title = input$idx,
          opacity = 1
        )
    })
  })
}