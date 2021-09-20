# load the data
ml_data <- read.csv('data/ml_predict_pca.csv')

# train
x <- as.matrix(ml_data[, 2:21])
y <- as.matrix(ml_data[, 1])

indexes = createDataPartition(ml_data$total_perc_change, p = .85, list = F)
train = ml_data[indexes, ]
test = ml_data[-indexes, ]

train_x = train[, 2:21]
train_y = train[, 1]

test_x = test[, 2:21]
test_y = test[, 1]

indx <- sapply(ml_data[, c(2:21)], is.factor)
ml_data[, c(2:21)][indx] <-
  lapply(ml_data[, c(2:21)][indx], function(x)
    as.numeric(as.character(x)))

mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(sidebarLayout(
    sidebarPanel(helpText(
      ns(
        "Principal Component Analysis compresses data features or dimensions from many to two, while preserving as much variance in the data as possible.
      This loadings plot shows how strongly each feature influences each principal component.
           The longer the loading, the more influencial the component.
           The loading angles reveal relationships amongst features.
           A small angle indicates the features are positively correlated. A 180 degree angle indicates a negatively correlated relationship, while a right angle suggests little to no correlation.
           Finally, loadings parallel to the PC1 axis indicate influence on the first principal component, while loadings parallel to the PC2 axis suggest influence on the second principal component."
      )
    )),
    mainPanel(plotOutput(ns("plot_pca"))),
  )))
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
        loadings.colour = 'darkred',
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