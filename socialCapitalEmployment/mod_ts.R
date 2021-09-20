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

employment <- read_csv("data/timeseries.csv")

mod_ts_ui <- function(id) {
  ns <- NS(id)
  tagList(fixedPage(fixedRow(
    column(
      2,
      selectizeInput(
        inputId = ns("dataset"),
        label = "Choose a county:",
        choices = c(unique(employment["countyfips"])),
        multiple = TRUE,
        selected = "Travis County, Texas",
        options = list(create = TRUE)
      )
    ),
    column(10, (echarts4r::echarts4rOutput(ns(
      "ts_plot"
    ))))
  )))
}

mod_ts_server <- function(id)  {
  moduleServer(id, function(input, output, session) {
    datasetInput <- reactive({
      employment %>% dplyr::filter(countyfips == input$dataset)
      
    })
    
    echarts4r::e_common(font_family = "Playfair Display",
                        theme = "walden")
    
    
    # plot time series
    output$ts_plot <- echarts4r::renderEcharts4r({
      dataset <- datasetInput()
      dataset %>% dplyr::arrange(date) %>% dplyr::group_by(countyfips) %>%
        e_charts(x = date) %>% echarts4r::e_line(emp_incbelowmed, symbol =
                                                   'none') %>%
        echarts4r::e_tooltip(trigger = "axis") %>%
        echarts4r::e_axis_labels("Days")
      
    })
    
  })
}