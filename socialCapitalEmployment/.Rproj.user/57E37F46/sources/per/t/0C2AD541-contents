#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(sf)
library(tigris)
library(fullPage)
library(caret)
library(echarts4r)
library(magrittr)
library(readr)
library(dplyr)
library(ggfortify)
library(ggthemes)
library(factoextra)
library(mlr)

source("mod_map.R")
source("mod_ts.R")
source("mod_pca.R")
source("mod_predict.R")

ui <- tagList(
  pagePiling(
    center = TRUE,
    sections.color = c("#7b959c", "#9b4228","#b9bdc9","#f6e7ea","#e2e2e2","#1e4356"),
    menu = c(
      "Home" = "home",
      "Map" = "map",
      "Series" = "ts",
      "PCA" = "pca",
      "Predict" = "predict",
      "About" = "about"
    ),
    pageSectionImage(
      center = TRUE,
      img = "",
      menu = "home",
      h1(("Employment & Social Capital across the U.S."), class = "header shadow-dark"),
      h3(
        class = "light footer",
        "by",
        tags$a("eoefelein", href = "https://github.com/eoefelein", class = "link")
      )
    ),
    pageSection(center = TRUE,
                menu = "map",
                mod_map_ui("map"),
                br()),
    pageSection(center = TRUE,
                menu = "ts",
                mod_ts_ui("ts"),
                br()),
    pageSection(center = TRUE,
                menu = "pca",
                mod_pca_ui("pca"),
                br()),
    pageSection(
      center = TRUE,
      menu = "predict",
      mod_predict_ui("predict"),
    ),
    pageSection(
      center = TRUE,
      menu = "about",
      h1("About", class = "header shadow-dark"),
      h2(
        class = "shadow-light",
        tags$a(
          "The Code",
          href = "https://github.com/eoefelein/COVID_Business_Recovery_and_Social_Capital",
          target = "_blank",
          class = "link"
        ),
        "|",
        tags$a(
          "The Data",
          href = "https://github.com/OpportunityInsights/EconomicTracker",
          target = "_blank",
          class = "link"
        )
      ),
      h3(
        class = "light footer",
        "by",
        tags$a("eoefelein", href = "https://github.com/eoefelein", class = "link")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  mod_map_server("map")
  mod_ts_server("ts")
  mod_pca_server("pca")
  mod_predict_server("predict")
  
}

# Run the application
shinyApp(ui = ui, server = server)