library(shiny)
ui <- fluidPage(title = "Binnr 2.0.1",

  tags$head(tags$style(HTML("
    .form-control {
      display: inline-block;
    }

    body {
      font-size: 12px;
    }"
    )
  )),

  fluidRow(
    column(12, DT::dataTableOutput("summary"))
  ),



  fluidRow(
      actionButton("collapse", " - ", width = '40px'),
      actionButton("expand", " + ", width = '40px'),
      actionButton("neutral", " != ", width = '40px'),
      div(style='display:inline-block',selectInput("mono", label = "Monotonicity", choices = -1:2, multiple = FALSE,
                         selected = 0, width = '60px', selectize = FALSE)),
      shiny::actionButton("save", "Save"),
    column(12, DT::dataTableOutput("bivariate"))
  ),

  fluidRow(
    column(12, verbatimTextOutput("debug"))
  )
)
