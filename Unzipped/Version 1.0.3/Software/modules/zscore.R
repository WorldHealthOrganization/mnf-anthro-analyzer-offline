zscoreOutput <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Z-scores",
    icon = icon("sort-amount-asc"),
    tags$head(tags$style("tfoot {display: table-header-group;}")),
    validatorOutput(ns("validation")),
    fixedRow(
      column(12, shinyjs::disabled(
        downloadButton(
          ns("dl_zscore"),
          label = "Download z-scores"
        )
      ))
    ),
    fixedRow(
      column(
        12,
        DT::dataTableOutput(ns("z"))
      )
    )
  )
}

zscore <- function(input, output, session, list_map_vars, age_in_months, dataset, df_matched_vars_strat, df_age_group, original_filename) {
  validation <- callModule(validatorModule, "validation", reactive_zscore_validation(dataset, list_map_vars))

  validated_dataset <- reactive({
    req(validation())
    dataset()
  })

  output$nrows <- renderText(nrow(validated_dataset()))

  # Run z-score calculations
  df_zscores <- reactive({
    if (validation()) {
      CalculateZScores(
        data = validated_dataset(),
        sex = list_map_vars[["sex"]](),
        weight = list_map_vars[["weight"]](),
        lenhei = list_map_vars[["lenhei"]](),
        lenhei_unit = list_map_vars[["lenhei_unit"]](),
        oedema = list_map_vars[["oedema"]]()
      )
    } else {
      NULL
    }
  })

  observeEvent(df_zscores(), {
    if (is.data.frame(df_zscores())) {
      shinyjs::enable("dl_zscore")
    } else {
      shinyjs::disable("dl_zscore")
    }
  })

  output$z <- DT::renderDataTable({
    req(is.data.frame(df_zscores()))
    data <- dplyr::select(df_zscores(), -uid)
    dplyr::mutate_if(data, is.numeric, dplyr::funs(round(., digits = 2)))
  }, options = list(paging = TRUE, pageLength = 10))

  # Download table
  # Merge filtered in and filtered out children for download file
  df_download_z <-
    reactive({
      req(is.data.frame(df_zscores()))
      bind_rows(
        mutate(df_zscores(), included = TRUE),
        df_age_group() %>%
          anti_join(df_zscores(), by = "uid") %>%
          mutate(included = FALSE)
      ) %>%
        arrange(uid) %>%
        select(-uid) %>%
        select(included, everything())
    })


  output$dl_zscore <- downloadHandler(
    filename = function() make_export_filename(original_filename(), "zscore.csv"),
    content = function(file) readr::write_csv(df_download_z(), file, na = "")
  )
}
