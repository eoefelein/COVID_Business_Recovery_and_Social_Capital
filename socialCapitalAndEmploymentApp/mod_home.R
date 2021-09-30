mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(headerPanel(
    h1(
      "Employment & Social Capital Across the U.S.",
      class = "header shadow-dark",
      align = "left"
    ),
  ),
  mainPanel(
    tags$div(
      class = "landing-wrapper",
      tags$div(class = "landing-block background-content",
               img(src = 'human-tower-competition-close-up.jpg')),
      tags$div(class = "landing-block foreground-content",
               h2(
                 class = "light footer",
                 align = "left",
                 "by",
                 tags$a("Erin Oefelein", href = "https://github.com/eoefelein", class = "link")
               ))
    )
  )))
}

mod_home_server <- function(id)  {
  moduleServer(id, function(input, output, session) {
    
  })
}