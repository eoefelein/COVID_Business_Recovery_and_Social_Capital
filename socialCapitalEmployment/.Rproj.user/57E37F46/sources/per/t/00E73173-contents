# library(shiny)
# library(magrittr)
# library(fullPage)
# library(readr)
# library(ggplot2)
# library(echarts4r)

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

employment <- read_csv("timeseries.csv")
# # convert to date
# employment <- within(employment, date <- as.Date(paste(employment$year, employment$month,
#                                                        employment$day), "%Y %m %d"))
# # subset for after lockdown anniversary
# emp_lockdown <- subset(employment, date > as.Date("2021-03-17") )
# # coerce to numeric
# cols.num <- c("emp", "emp_incq1", "emp_incq2", "emp_incq3", "emp_incq4", "emp_incmiddle",
#               "emp_incbelowmed", "emp_incabovemed")
# emp_lockdown[cols.num] <- sapply(emp_lockdown[cols.num],as.numeric)
# # pivot wider
# emp_lockdown %>%
#   dplyr::select(date,countyfips,emp_incbelowmed) %>%
#   pivot_wider(names_from = countyfips, values_from = emp_incbelowmed) -> pivot
# # remove cols with more than 50% NANs
# pivot[, which(colMeans(!is.na(pivot)) > 0.5)] -> pivot
#
# #ECHARTS
# echarts_test <- pivot_longer(pivot, cols = 2:1190, names_to = "countyfips", values_to = "emp_incbelowmed")

mod_ts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fixedPage(
      h2("Employment by County", align = "center"),
      fixedRow(
        column(
          4,
          selectizeInput(
            inputId = ns("dataset"),
            label = "Choose a county:",
            choices = c(unique(employment["countyfips"])),
            multiple = TRUE,
            selected = "Travis County, Texas",
            options = list(create = TRUE)
            )
          ),
        column(
          8, (echarts4r::echarts4rOutput(ns("ts_plot")))
          )
        )
      )
    )
}

mod_ts_server <- function(id)  {
  
  moduleServer(id, function(input, output, session) {
    
    datasetInput <- reactive({
      employment %>% dplyr::filter(countyfips == input$dataset)
      
    })
    
    # # Generate a summary of the dataset ----
    # output$summary <- renderPrint({
    #   dataset <- datasetInput()
    #   summary(dataset$emp_incbelowmed)
    # })
    
    echarts4r::e_common(font_family = "Playfair Display",
                        theme = "walden") # "infographic", "vintage", "chalk"
    
    
    # plot time series
    output$ts_plot <- echarts4r::renderEcharts4r({
      
      dataset <- datasetInput()
      dataset %>% dplyr::arrange(date) %>% dplyr::group_by(countyfips) %>%
        e_charts(x = date) %>% echarts4r::e_line(emp_incbelowmed, symbol =
                                                   'none') %>%
        echarts4r::e_tooltip(trigger = "axis") %>%
        echarts4r::e_axis_labels("Days")
      
      # ggplot(dataset, aes(x = date, y=emp_incbelowmed, group=countyfips, color = countyfips)) + geom_line(stat='identity', width = 0.5) + scale_y_reverse() + theme_minimal()
      
    })
    
  })
}
# shiny::shinyApp(ui = ui, server = server)