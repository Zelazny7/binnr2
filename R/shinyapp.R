library(shiny)
library(miniUI)

App <- function(bin) {
  shinyApp(
  ui = fluidPage(

    shiny::actionButton("expand", "Expand"),

    shiny::actionButton("collapse", "Collapse"),

    shiny::textOutput("test"),

    shiny::textOutput("out"),

    DT::dataTableOutput('tbl')
    ),

  server = function(input, output) {
    v <- reactiveValues(bin=bin)

    observeEvent(input$collapse, {
      #print(input$tbl_rows_selected)
      tmp <- as.data.frame(v$bin)
      v$bin <- v$bin - which(rownames(tmp) %in% input$tbl_rows_selected)
    })

    observeEvent(input$expand, {
      tmp <- as.data.frame(v$bin)
      v$bin <- v$bin + which(rownames(tmp) %in% input$tbl_rows_selected)
    })

    output$tbl = DT::renderDataTable(
      as.data.frame(v$bin),
      options = list(lengthChange = FALSE))
    }
  )
}
