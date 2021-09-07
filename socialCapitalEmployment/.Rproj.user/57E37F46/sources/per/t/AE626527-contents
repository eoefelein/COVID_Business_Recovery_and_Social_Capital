# library(tidyverse)
# library(dplyr)
# library(ggplot2)
# library(ggfortify)
# library(ggthemes)
# library(caret)
# library(factoextra)
# library(mlr)

ml_data <- read.csv('ml_predict_pca.csv')

# train
x <- as.matrix(ml_data[, 2:21])
y <- as.matrix(ml_data[, 1])

indexes = createDataPartition(ml_data$total_perc_change, p = .85, list = F)
train = ml_data[indexes,]
test = ml_data[-indexes,]

train_x = train[, 2:21]
# train_x = scale(train_x)[,]
train_y = train[, 1]

test_x = test[, 2:21]
# test_x = scale(test[, 2:20])[,]
test_y = test[, 1]

indx <- sapply(ml_data[, c(2:21)], is.factor)
ml_data[, c(2:21)][indx] <-
  lapply(ml_data[, c(2:21)][indx], function(x)
    as.numeric(as.character(x)))

mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fixedPage(
      plotOutput(ns("plot_pca"))))
}

mod_pca_server <- function(id)  {
  
  moduleServer(id, function(input, output, session) {
    
    output$plot_pca <- renderPlot({
      
      pca <- ml_data[, c(3:22)] %>%
        prcomp(scale. = TRUE)
      
      autoplot(
        pca,
        data = ml_data[, c(3:22)],
        loadings = TRUE,
        loadings.colour = 'darkgray',
        loadings.label = TRUE,
        loadings.label.colour = 'black',
        loadings.label.size = 5
      ) +
        theme_stata() +
        labs(title = "Principal Component Analysis (PCA)",
             caption = "Source: Organization for Economic Co-operation and Develpoment (OECD)")
      
    })
    
  })
}