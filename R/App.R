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

    ## values that should trigger updates when changed
    values <- reactiveValues(summary=su, i=1, bins=bins)

    output$summary <- DT::renderDataTable({
      DT::datatable(
        values$summary,
        style = "bootstrap",
        selection = list(mode="single", target="row"),
        options = list(
          pageLength = 5,
          displayStart = values$i - 1
        ))
    }, server = TRUE)

    observeEvent(input$summary_row_last_clicked, {
      values$i <- input$summary_row_last_clicked
    })

    proxy <- DT::dataTableProxy('summary')

    observeEvent(input$summary_row_last_clicked, {
      DT::selectRows(proxy, values$i)
    })

    output$debug <- renderText({
      z <- input$summary_row_last_clicked_focus
      print(z)
    })

    observeEvent(input$save, {
      stopApp(values$bins)
    })

    ### DROP ###
    observeEvent(input$drop, {
      dropped(values$bins[[values$i]]) <- !dropped(values$bins[[values$i]])
      values$summary[values$i,] <- summary(values$bins[[values$i]])
    })

    ### COLLAPSE ###
    observeEvent(input$collapse, {
      v <- input$bivariate_rows_selected
      v <- seq(min(v), max(v))
      values$bins[[values$i]] <- values$bins[[values$i]] - v
    })

    ### EXPAND ###
    observeEvent(input$expand, {
      v <- input$bivariate_row_last_clicked
      values$bins[[values$i]] <- values$bins[[values$i]] + v
    })

    observeEvent(input$mono, {
      if (!is.null(values$i)) {
        values$bins[[values$i]] <- mono(values$bins[[values$i]],
          as.numeric(input$mono))
      }
    })

    observeEvent(input$dropped, {
      print("holy crap that worked")
    })

    ### HEADER ###
    output$info <- renderUI({
      get.header(values$bins[[values$i]])
    })

    output$bivariate <- DT::renderDataTable({

      tbl <- as.data.frame(values$bins[[values$i]])
      tbl$WoE <- add.tags(tbl$WoE)

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
    }, server = TRUE)
  }
}


checkbox <- function(.slot) {
  HTML(sprintf("<input type='checkbox' disabled %s>",
    ifelse(.slot, "checked", "")))
}

get.header <- function(object) {
  tags$div(
    tags$b(object@name),
    tags$br(),
    "Drop: ", checkbox(object@drop),
    "In Model: ", checkbox(object@inmodel),
    "New: ", checkbox(object@new),
    "Step 2: ", checkbox(object@steptwo),
    "Approved: ", checkbox(object@approved)
  )
}

adjust2 <- function(bins) {
  require(shiny)

  ## access data from the app?

  runApp(shinyApp(
    ui = ui,
    server = fun(bins)
  ))
}
