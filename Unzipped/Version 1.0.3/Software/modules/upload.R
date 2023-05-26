UploadInput <- function(id) {
  ns <- NS(id)
  fileInput(ns("data_upload"),
    HTML("Upload data<br>(.csv format)"),
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  )
}

Upload <- function(input, output, session) {
  reactive({
    if (!is.null(input$data_upload$name)) {
      valid_filename <- grepl(
        x = tolower(input$data_upload$name),
        pattern = "^[a-zA-Z0-9_\\-]{1,220}\\.csv$"
      )
      if (!valid_filename) {
        try(file.remove(input$data_upload$datapath))
      }
      # validation messages are done in app.R
    }
    input$data_upload
  })
}
