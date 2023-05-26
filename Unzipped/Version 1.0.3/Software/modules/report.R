ReportOutput <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Summary Report",
    icon = icon("book"),
    fluidRow(column(12, validatorOutput(ns("validation")))),
    p(
      "This function allows to export a template summary report in Word laying out guidance on minimum required details to follow good practice in reporting. The report also includes main findings (graphics and tables) regarding prevalence estimates by different disaggregation factors for the five main indicators, namely stunting, wasting, severe wasting, overweight and underweight, as well as some data quality assessment statistics."
    ),

    shinyjs::disabled(downloadButton(ns("report"),
                                     label = "Generate & download report",
                                     class = "btn-primary"))
  )
}

Report <- function(input, output, session, df_filtered,
                   age_in_months, list_map_vars, df_raw, original_filename,
                   number_removed_due_to_age) {

  # input validation
  stopifnot(is.reactive(df_filtered))
  stopifnot(is.reactive(age_in_months))
  stopifnot(is.list(list_map_vars))
  stopifnot(is.reactive(df_raw))
  stopifnot(is.reactive(original_filename))
  stopifnot(is.reactive(number_removed_due_to_age))

  inc_progress <- function() {
    shiny::incProgress()
  }

  validation <- callModule(
    validatorModule,
    "validation",
    anthro_create_prevalence_validation(list_map_vars, df_filtered)
  )

  observe({
    preconditions_ok <- validation()
    if (preconditions_ok) {
      shinyjs::enable("report")
    } else {
      shinyjs::disable("report")
    }
    shinyjs::toggle("precondition-explanation", condition = !preconditions_ok)
  })

  output$report <-
    downloadHandler(
      filename = function() {
        make_export_filename(original_filename(), "report.docx")
      },

      content = function(file) {
        req(validation())

        # the environment in which the report is run
        # these are the dependencies
        render_environment <- rlang::new_environment(
          list(
            inc_progress = inc_progress,
            df_filtered = df_filtered,
            age_in_months = age_in_months,
            list_map_vars = list_map_vars,
            df_raw = df_raw,
            zscore_flag_cols = zscore_flag_cols,
            number_removed_due_to_age = number_removed_due_to_age
          )
        )
        parent.env(render_environment) <- globalenv()

        withProgress(
          message = "Generating report",
          detail = "Please be patient - This may take a while...",
          value = 0, {
            suppressMessages(
              suppressWarnings(
                rmarkdown::render(
                  input = "reports/report.Rmd",
                  output_format =
                    rmarkdown::word_document(reference_docx = "word-styles-reference.docx"),
                  output_file = file, envir = render_environment,
                  intermediates_dir = file.path(tempdir(), digest::sha1(runif(1))),
                  quiet = TRUE
                )
              )
            )
          }
        )
      }
    )
}
