library(shiny)
ui <- fluidPage(
  titlePanel("Binnr 2.0.1"),

  fluidRow(
    column(12, DT::dataTableOutput("summary"))
  ),

  fluidRow(
    column(12, DT::dataTableOutput("Bivariate"))
  ),

  fluidRow(
    column(12, verbatimTextOutput("debug"))
  )
)
