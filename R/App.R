library(shiny)
add.tags <- function(v) {

  ## how much of the first should be blanked out
  p1 <- ifelse(v > 0, 50, 50 - 100 * (v/min(v)/2))
  p2 <- ifelse(v <= 0, 50 - p1, 100 * (v/max(v)/2))
  p3 <- ifelse(v <= 0, 50, 50 - p2)
  color <- ifelse(v > 0, 'lightcoral', 'lightblue')

  sprintf(
    paste0(
      "<div style='position:relative;'>",
      "<div style='position:relative;z-index:1;'>%0.4f</div>",
      "<div style='position:absolute;top:0;bottom:0;left:0;width:%s%%;'></div>",
      "<div style='position:absolute;top:0;bottom:0;left:%s%%;width:%s%%;background-color:%s;'></div>",
      "<div style='position:absolute;top:0;bottom:0;right:0;width:%s%%;'></div>",
      "</div>"
    ), v, p1, p1, p2, color, p3)
}

## function that wraps the server function
fun <- function(bins) {
  su <- summary(bins)

  function(input, output) {

    output$summary <- DT::renderDataTable({
      DT::datatable(
        su,
        style = "bootstrap",
        extensions = 'KeyTable',
        selection = list(mode="single", selected=1, target="row"),
        options = list(
          pageLength = 5,
          keys = TRUE,
          blur = TRUE)
        )
    })

    output$debug <- renderText({
      #browser()
      i <- input$summary_row_last_clicked
      z <- input$summary_row_last_clicked_focus
      print(i)
      print(z)
    })

    observeEvent(input$save, {
      stopApp(bins)
    })

    observeEvent(input$drop, {
      i <- input$summary_row_last_clicked
      dropped(bins[[i]]) <- !dropped(bins)
    })


    observeEvent(input$collapse, {
      #browser()
      i <- input$summary_row_last_clicked
      v <- input$bivariate_rows_selected
      v <- seq(min(v), max(v))
      print(v)
      bins[[i]] <<- bins[[i]] - v
    })

    observeEvent(input$expand, {
      browser()
      i <- input$summary_row_last_clicked
      v <- input$bivariate_row_last_clicked
      print(v)
      bins[[i]] <<- bins[[i]] + v
    })

    observeEvent(input$mono, {
      i <- input$summary_row_last_clicked
      if (!is.null(i)) bins[[i]] <<- mono(bins[[i]], as.numeric(input$mono))
    })

    output$bivariate <- DT::renderDataTable({

      input$collapse
      input$expand
      input$mono
      i <- input$summary_row_last_clicked
      if (is.null(i)) i <- 1

      #browser()

      tbl <- as.data.frame(bins[[i]])
      tbl$WoE <- add.tags(tbl$WoE)

      ## create the javascript

      DT::formatRound(
      DT::formatPercentage(
        DT::datatable(
          tbl,
          style = "bootstrap",
          selection = "multiple",
          filter = "none",
          escape = 8,
          options = list(
            paging = FALSE,
            ordering = FALSE,
            searching = FALSE,
            info = FALSE,
            columnDefs = list(
            list(
              targets = 10,
              visible = FALSE),
            list(
              targets=8,
              className = 'dt-center',
              width= '200px')
            )
          )
        ),
          columns = 4:7
        ),
          columns = 9:10, 3
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
