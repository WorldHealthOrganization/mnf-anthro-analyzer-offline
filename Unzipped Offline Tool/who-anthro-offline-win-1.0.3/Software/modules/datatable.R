DFTableOutput <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Dataset",
    icon = icon("table"),
    tags$style(type = "text/css", ".row {margin-top: 15px;}"),
    tags$head(tags$style("tfoot {display: table-header-group;}")), # puts filter at top of table
    fluidRow(
      uiOutput(ns("warning")),
      column(12, DT::dataTableOutput(ns("table")))
    )
  )
}

DFTable <- function(input, output, session, dataset, num_removed_na_rows) {
  stopifnot(is.reactive(num_removed_na_rows))
  output$table <- DT::renderDataTable({
    validate(
      need(is.data.frame(dataset()), "The uploaded data does not seem to be a table"),
      need(nrow(dataset()) > 0, "The data needs to have at least one row")
    )
    dplyr::select(dataset(), -uid)
  }, options = list(paging = TRUE, pageLength = 10))

  output$warning <- renderUI({
    n <- num_removed_na_rows()
    if (n > 0L) {
      rows <- if (n == 1L) "row" else "rows"
      n_rows <- paste0(n, " ", rows)
      list(
        column(12, div(
          class = "alert alert-warning",
          h4(class = "alert-heading", "Removed rows from dataset"),
          p(paste0(
            "The dataset contained ", n_rows, " with all",
            " columns having missing values. These ", rows,
            " have been removed from the dataset."
          ))
        ))
      )
    } else {
      list()
    }
  })
}
