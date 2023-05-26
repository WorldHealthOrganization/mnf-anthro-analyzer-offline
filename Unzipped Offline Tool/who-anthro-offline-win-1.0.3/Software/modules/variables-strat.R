StratVariablesInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_variables"))
}

StratVariables <-
  function(input,
           output,
           session,
           label = NULL,
           multiple = FALSE,
           choices = NULL,
           selected = NULL,
           show_element = NULL) {
    stopifnot(shiny::is.reactive(choices))
    output$ui_variables <- renderUI({
      ns <- session$ns

      if (is.null(show_element) ||
          (is.reactive(show_element) && isTruthy(show_element()))) {
        selectInput(
          ns("slct_variables"),
          label = label,
          selected = selected,
          choices = choices(),
          multiple = multiple
        )
      } else {
        list()
      }
    })

    reactive(input$slct_variables)
  }
