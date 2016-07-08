library(shiny)

styleColorBar2 <- function(data){
  r1 = min(data)
  r2 = max(data)

  ## Create three different kinds of JS depending on the value
  JS(gsub("\\n", "", sprintf("
       value < 0 ?
      'linear-gradient(90deg, transparent, red ' + (50 - 100*(value/%s/2)) + '%%, transparent 50%%)'
      :
      'linear-gradient(90deg, transparent , blue 50%%, transparent ' + (50 + 100*(value/%s/2)) + '%%)'",
    r1, r2)))
}

## function that wraps the server function
fun <- function(bins) {
  su <- summary(bins)
  i <- 1


  function(input, output) {

    output$summary <- DT::renderDataTable(
      DT::datatable(
        su,
        style = "bootstrap",
        selection = list(
          mode = "single",
          selected = 1)
        )
    )

    output$debug <- renderText(input$summary_row_last_clicked)

    output$Bivariate <- DT::renderDataTable({

      i <- as.integer(input$summary_row_last_clicked)


      tbl <- as.data.frame(bins[[i]])
      v <- tbl$Pred

      ## create the javascript




      DT::formatStyle(
      DT::formatRound(
      DT::formatPercentage(
        DT::datatable(
          tbl,
          style = "bootstrap",
          selection = "none",
          filter = "none",
          options = list(
            columnDefs = list(
            list(
              targets = 10,
              visible = FALSE),
            list(
              targets=8,
              width= '200px')
            )
          )
        ),
          columns = 4:7
        ),
          columns = 8:10, 3
        ),
          columns = 8,
          valueColumns = 10,
          background =  styleColorBar2(v),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )

    })

    # output$Plot <- renderPlot({
    #
    #   i <- as.integer(input$summary_row_last_clicked)
    #   plot(bins[[i]])
    # })
  }
}



adjust2 <- function(bins) {
  require(shiny)

  ## access data from the app?

  runApp(shinyApp(
    ui = ui,
    server = fun(bins)
  ))
}
