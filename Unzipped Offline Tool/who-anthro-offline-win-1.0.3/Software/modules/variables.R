VariablesInput <- function(id) {
  ns <- NS(id)

  htmlOutput(ns("ui_variables"))
}

Variables <- function(input, output, session, label = NULL, multiple = FALSE,
                      choices = NULL,
                      selected = NULL,
                      popover_title = reactive(NULL),
                      popover_content = reactive(NULL)) {
  stopifnot(is.null(choices) || shiny::is.reactive(choices)
  || is.character(choices))
  stopifnot(is.reactive(popover_title) && is.reactive(popover_content))
  ns <- session$ns

  choice_input <- reactive({
    choice_input <- if (shiny::is.reactive(choices)) {
      choices()
    } else if (is.character(choices)) {
      choices
    } else {
      NULL
    }
  })

  output$ui_variables <- renderUI({
    element <- selectInput(
      inputId = ns("slct_variables"),
      label = HTML(label),
      choices = choice_input(),
      selected = selected,
      multiple = multiple
    )
    p_title <- popover_title()
    p_content <- popover_content()
    if (!is.null(p_title) && !is.null(p_content)) {
      element$attribs$title <- p_title
      element$attribs$`data-toggle` <- "popover"
      element$attribs$`data-content` <- p_content
    } else {
      element$attribs$title <- NULL
      element$attribs$`data-toggle` <- NULL
      element$attribs$`data-content` <- NULL
    }
    tooltip_code <- paste0('$(function () {
                             $("#', ns("ui_variables"), ' > :first").popover({"trigger": "hover"})
  })')
    list(
      element,
      tags$script(HTML(tooltip_code))
    )
  })

  reactive({
    input$slct_variables
  })
}
