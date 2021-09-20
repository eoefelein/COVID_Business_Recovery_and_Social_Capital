# load the data
df <- read.csv('data/ml_predict_data.csv')

MinMaxScaling <- function(x)
{
  return ((x - min(x)) / (max(x) - min(x)))
}

# take out non-scaled data
perc_change <- df$avg_perc_change
county_name <- df[, 1]

final_df <- df[,3:22]
# applying scaling column wise to X columns 3:22
df <- as.data.frame(apply(final_df, 2, MinMaxScaling)) 

# add back non-scaled data
df$avg_perc_change <- perc_change
rownames(df) <- county_name

mod_predict_ui <- function(id) {
  ns <- NS(id)
  tagList(fixedPage(fixedRow(
    column(
      2,
      
      selectizeInput(
        inputId = ns("county"),
        label = "Choose a county:",
        choices = c(unique(rownames(df))),
        multiple = FALSE,
        selected = "Travis County, Texas",
        options = list(create = TRUE)
      )
    ),
    column(10,
           echarts4r::echarts4rOutput(ns("predict_plot")), )
  )))
}

mod_predict_server <- function(id)  {
  moduleServer(id, function(input, output, session) {
    
    countyInput <- reactive({
      df[rownames(df) == input$county,]
      
    })
    
    output$predict_plot <- echarts4r::renderEcharts4r({
      
      # take out selected county
      df <- df[rownames(df) != input$county,]
      
      # create train and test data
      indexes = createDataPartition(df$avg_perc_change, p = .85, list = F)
      train = df[indexes, ]
      train_x = train[, 2:21]
      train_y = train[, 1]
      
      test = df[-indexes, ]
      test <- rbind(test, countyInput()) # add selected county data to test
      test_x = test[, 2:21]
      test_y = test[, 1]
      
      # Run k-NN:
      set.seed(400)
      ctrl <- trainControl(method = "repeatedcv", repeats = 3)
      # make prediction
      knnFit <-
        caret::train(
          avg_perc_change ~ .,
          data = train,
          method = "knn",
          trControl = ctrl,
          tuneLength = 20
        )
      # create data.frame from prediction data
      prediction <-
        data.frame("2021-09-22",
                   input$county,
                   predict(knnFit, newdata = countyInput()))
      print(prediction)
      
      # add prediction to timeseries (ts) data
      data <- read.csv('data/plotting_predictions.csv')
      print(data)
      # drop X col
      drops <- c("X")
      data <- data[, !(names(data) %in% drops)]
      
      # filter by selected county
      selected_county_emp_ts <- data %>% filter(countyname == input$county)
      
      # assign colnames of selected_county_emp_ts to prediction data
      names(prediction) <- colnames(selected_county_emp_ts)
      # combine past ts data with predicted, future ts data
      past_and_future_ts <- rbind(selected_county_emp_ts, prediction)
      
      # to plot the data, convert to date 
      past_and_future_ts$date <- as.Date(past_and_future_ts$date)
      # order by date
      selected <- past_and_future_ts[order(past_and_future_ts$date),]
      
      ts_df <- selected %>%
        e_charts(x = date) %>%
        e_datazoom(show = FALSE, toolbox = FALSE) %>%
        e_x_axis(date,
                 axisPointer = list(show = TRUE),
                 borderWidth = 2)
      
      ts_df %>% e_bar(emp_incbelowmed) %>%
        e_legend(FALSE)
      
    })
    
  })
}