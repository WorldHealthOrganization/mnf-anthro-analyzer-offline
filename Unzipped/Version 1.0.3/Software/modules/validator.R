validatorOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("validationBox"))
}

validatorModule <- function(input, output, session, validations) {
  stopifnot(is.reactive(validations))

  all_valid <- reactive({
    validation_result <- validations()
    stopifnot(is.list(validation_result))
    stopifnot(length(validation_result$type) == 1L)
    if (validation_result$type == "ok") {
      TRUE
    } else {
      FALSE
    }
  })

  output$validationBox <- renderUI({
    if (all_valid()) {
      HTML("")
    } else {
      error_message <- isolate(validations()$message)
      alert_ui <- div(
        class = "alert alert-warning",
        h4("Before using this module, please fix the error below",
          class = "alert-heading"
        ),
        p(error_message)
      )
      fluidRow(column(12, alert_ui))
    }
  })

  all_valid
}

# helper function

check_validation_conditions <- function(...) {
  conditions <- list(...)
  for (cond in conditions) {
    if (cond$is_violated) {
      return(cond$msg)
    }
  }
  validation_ok()
}

condition <- function(cond, msg) {
  stopifnot(is.logical(cond), length(cond) == 1L)
  list(
    is_violated = cond,
    msg = msg
  )
}

validation_ok <- function() {
  list(
    type = "ok"
  )
}

validation_error <- function(msg) {
  list(
    type = "error",
    message = msg
  )
}
