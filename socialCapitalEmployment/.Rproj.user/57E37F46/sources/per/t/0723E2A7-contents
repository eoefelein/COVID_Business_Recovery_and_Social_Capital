# library(leaflet)
# library(tidyverse)
# library(ggplot2)
# library(caret)
# library(echarts4r)

df <-
  read.csv('ml_predict_data.csv')

MinMaxScaling <- function(x)
{
  return ((x - min(x)) / (max(x) - min(x)))
}

perc_change <- df$total_perc_change
county_name <- df[, 1]

# drops <- c("ï..",'county_name')
final_df <- df[,-1]

df <- as.data.frame(apply(final_df, 2, MinMaxScaling))
df$total_perc_change <- perc_change
rownames(df) <- county_name

mod_predict_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fixedPage(
      fixedRow(
        column(
          4,
          selectizeInput(
            inputId = ns("county"),
            label = "Choose a county:",
            choices = c(unique(rownames(df))),
            multiple = FALSE,
            selected = "Travis County, Texas",
            options = list(create = TRUE)
          )
        ),
        column(8,
               echarts4r::echarts4rOutput(ns("predict_plot")),
               # plotOutput("predict_plot"))),
               )
        )
      )
    )
}

mod_predict_server <- function(id)  {
  
  moduleServer(id, function(input, output, session) {
    
    countyInput <- reactive({
      df[rownames(df) == input$county,]
      
    })
    
    ## echarts4r::renderEcharts4r
    output$predict_plot <- echarts4r::renderEcharts4r({
      
      df <- df[rownames(df) != input$county,]
      
      indexes = createDataPartition(df$total_perc_change, p = .85, list = F)
      train = df[indexes, ]
      train_x = train[, 2:21]
      train_y = train[, 1]
      
      test = df[-indexes, ]
      test <- rbind(test, countyInput())
      test_x = test[, 2:21]
      test_y = test[, 1]
      
      # Run k-NN:
      set.seed(400)
      ctrl <- trainControl(method = "repeatedcv", repeats = 3)
      knnFit <-
        caret::train(
          total_perc_change ~ .,
          data = train,
          method = "knn",
          trControl = ctrl,
          tuneLength = 20
        )
      prediction <-
        data.frame("2021-09-27",
                   input$county,
                   predict(knnFit, newdata = countyInput()))
      
      data <-
        read.csv(
          'plotting_predictions.csv'
        )
      print(data)
      drops <- c("X")
      data[, !(names(data) %in% drops)] -> data
      data %>% filter(countyfips == input$county) -> test
      
      
      names(prediction) <- colnames(data)
      rbind(test, prediction) -> test
      test$date <- as.Date(test$date)
      test <- test[order(test$date),]
      
      ts_df <- test %>%
        e_charts(x = date) %>%
        e_datazoom(type = "slider",
                   toolbox = FALSE,
                   bottom = -5) %>%
        e_tooltip() %>%
        e_x_axis(date, axisPointer = list(show = TRUE))
      
      ts_df %>% e_bar(emp_incbelowmed) %>%
        e_legend(FALSE)
      # %>%
      # e_visual_map_range(
      #   selected = list(as.Date(2021-09-27, origin="2021-03-17")),
      #   color = "green",
      #   bottom = 300
      # )
      
      # ggplot(test, aes(x=date, y=emp_incbelowmed)) +
      #   geom_area() +
      #   xlab("")
      # ggplot(test, aes(x = date, y=emp_incbelowmed)) +
      #   geom_bar(stat='identity', width = 1.0) +
      #   theme_light()
    })
    
  })
}
#
# # Create Shiny app ----
# shinyApp(ui = ui, server = server)