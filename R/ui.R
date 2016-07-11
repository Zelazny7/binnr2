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
    #column(12, tableOutput("summary"))
  ),

  fluidRow(
    column(12, shiny::htmlOutput("info", style='display:inline-block'))
  ),

  fluidRow(
    actionButton("collapse", " - ", width = '40px'),
    actionButton("expand", " + ", width = '40px'),
    actionButton("neutral", " != ", width = '40px'),

    div(style='display:inline-block',radioButtons("mono", label = "Monotonicity", choices = -1:2, inline=TRUE)),

    actionButton("drop", "Drop"),
    actionButton("save", "Save"),

    #actionButton("previous", "<=", width = '40px'),
    #actionButton("next", "=>", width = '40px'),

    column(12, DT::dataTableOutput("bivariate"))
  ),

  fluidRow(
    column(12, verbatimTextOutput("debug"))
  )
)
