# A module that displays a selectbox to select one variable of the
# already mapped variables (or a subset of those)
# It returns a reactive value based on the user input

MappingSelectorOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("mapping_select_ui"))
}

MappingSelector <- function(input,
                            output,
                            session,
                            label,
                            available_choices,
                            show_select_box,
                            list_map_vars) {
  stopifnot(is.list(list_map_vars), available_choices %in% names(list_map_vars))
  stopifnot(is.character(names(available_choices)))
  stopifnot(vapply(list_map_vars, is.reactive, logical(1L)))
  stopifnot(is.reactive(show_select_box))
  stopifnot(is.character(label), length(label) == 1L)

  ns <- session$ns

  select_choices <- reactive({
    req(show_select_box())
    choices <- Filter(function(col_name) {
      is_variable_mapped(col_name())
    }, list_map_vars[available_choices])

    available_choices_mapped <- available_choices %in% names(choices)
    choice_names <- names(available_choices[available_choices_mapped])

    # materialize
    dynamic_choices <- setNames(lapply(choices, function (x) x()),
                                choice_names)
    c("None" = "none", dynamic_choices)
  })

  select_input <- reactive({
    selectInput(ns("choices"), label = label, choices = isolate(select_choices()))
  })

  output$mapping_select_ui <- renderUI({
    if (isTRUE(show_select_box())) {
      select_input()
    } else {
      tagList()
    }
  })

  observe({
    req(show_select_box())
    choices <- select_choices()
    updateSelectInput(session, "choices", choices = choices)
  })

  reactive({
    req(input$choices %in% select_choices())
    list(
      label = names(which(select_choices() == input$choices)),
      column = input$choices
    )
  })
}

