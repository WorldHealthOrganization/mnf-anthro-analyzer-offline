ZscoreDataQualityModuleOutput <- function(id) {
  ns <- NS(id)
  list(
    tabPanel(
      "Z-score distribution",
      icon = icon("table"),
      validatorOutput(ns("validation_dist")),
      fixedRow(
        column(6, shinyjs::disabled(
          downloadButton(ns("dl_zdist"), label = "Download figure")
        )),
        column(6, VariablesInput(ns("id_variables_strat_z_zscore")))
      ),
      uiOutput(ns("dynamic_plot_zdist"))
    ),
    tabPanel(
      "Z-score flags",
      icon = icon("table"),
      validatorOutput(ns("validation_flags")),
      fixedRow(column(
        12,
        shinyjs::disabled(
          downloadButton(ns("dl_flag"), label = "Download figure")
        )
      )),
      plotOutput(ns("plot_flag"), height = "600px")
    ),
    tabPanel(
      "Z-score summary",
      icon = icon("table"),
      validatorOutput(ns("validation_summary")),
      fixedRow(column(12, shinyjs::disabled(
        downloadButton(ns("dl_zscore_summary"),
          label = "Download Z-score summary"
        )
      ))),
      fixedRow(column(12, DT::dataTableOutput(ns("zscore_summary"))))
    )
  )
}

ZscoreDataQualityModule <- function(input, output, session,
                                    list_map_vars,
                                    age_in_months,
                                    dataset,
                                    df_matched_vars_strat,
                                    original_filename) {
  ns <- session$ns

  # the same validation is used at three different places
  callModule(validatorModule, "validation_dist", reactive_zscore_validation(dataset, list_map_vars, height_and_weight = TRUE))
  callModule(validatorModule, "validation_flags", reactive_zscore_validation(dataset, list_map_vars, height_and_weight = TRUE))
  validation <- callModule(validatorModule, "validation_summary", reactive_zscore_validation(dataset, list_map_vars, height_and_weight = TRUE))

  validated_dataset <- reactive({
    req(validation())
    dataset()
  })

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

  variable_strat_z <-
    callModule(StratVariables, "id_variables_strat_z_zscore",
      choices = reactive(c("None", df_matched_vars_strat()$key)),
      label = "Stratification variable", selected = "None"
    )

  zscores_dist_data <- reactive({
    req(variable_strat_z())
    req(is.data.frame(df_zscores()))
    zscore_prepare_plot_dist_data(
      df_zscores = df_zscores(),
      stratification_var_name = variable_strat_z(),
      list_map_vars = list_mapping_to_char(list_map_vars)
    )
  })

  validate_zscore_plot <- function(n_levels) {
    validate(
      need(is.na(n_levels) || n_levels < 30, "This plot is only available for less than 30 groups")
    )
  }

  zscores_dist_plot <-
    reactive({
      data <- zscores_dist_data()
      if (!is.null(data[["n_levels"]]) &&
          !is.na(data[["n_levels"]]) && data[["n_levels"]] > 0) {
        validate_zscore_plot(data[["n_levels"]])
      }
      zscore_plot_distribution(data)
    })

  zscores_wo_flagged <- reactive({
    req(is.data.frame(df_zscores()))
    set_flagged_zscores_to_na(df_zscores())
  })

  plot_zdist_height <- reactive({
    data <- zscores_dist_data()
    n_levels <- data[["n_levels"]]
    validate_zscore_plot(n_levels)
    pmax(
      800,
      n_levels * 120,
      na.rm = TRUE
    )
  })

  output$plot_zdist <- renderPlot({
    zscores_dist_plot()
  })

  output$dynamic_plot_zdist <- renderUI({
    plotOutput(ns("plot_zdist"), height = paste0(plot_zdist_height(), "px"))
  })

  observeEvent(df_zscores(), {
    if (is.data.frame(df_zscores())) {
      fun <- shinyjs::enable
    } else {
      fun <- shinyjs::disable
    }
    fun("dl_zdist")
    fun("dl_flag")
    fun("dl_zscore_summary")
  })

  # Download plot
  output$dl_zdist <- downloadHandler(
    filename = function() make_export_filename(original_filename(), "zscores_distributions.png"),
    content = function(file) {
      png(file, height = plot_zdist_height(), width = 800)
      print(zscores_dist_plot())
      dev.off()
    }
  )

  flagged_zscores_plot <- reactive({
    df_zscores <- df_zscores()
    req(is.data.frame(df_zscores))
    zscore_plot_flagged_zscores(df_zscores)
  })

  output$plot_flag <- renderPlot({
    flagged_zscores_plot()
  })

  # Download plot
  output$dl_flag <- downloadHandler(
    filename = function() make_export_filename(original_filename(), "flagged_zscores.png"),
    content = function(file) {
      png(file, width = 800)
      print(flagged_zscores_plot())
      dev.off()
    }
  )

  # zscore summary
  zscore_summary_data <- reactive({
    req(is.data.frame(zscores_wo_flagged()))
    zscore_compute_summary_data(
      zscores_wo_flagged(),
      zscore_summary_groups(list_mapping_to_char(list_map_vars))
    )
  })

  output$dl_zscore_summary <- downloadHandler(
    filename = function() make_export_filename(original_filename(), "zscore_summary.csv"),
    content = function(file) {
      readr::write_csv(zscore_summary_data(), file, na = "")
    }
  )

  output$zscore_summary <- DT::renderDataTable({
    data <- zscore_summary_data()
    col_names <- colnames(data)

    res <- DT::datatable(data,
      selection = "none", rownames = FALSE,
      options = list(
        paging = FALSE,
        digits = 2,
        bFilter = FALSE
      )
    )
    color1 <- "#f9f9f9"
    color2 <- "white"
    res <- DT::formatStyle(res, col_names[1L:2L], backgroundColor = color1)
    color_map <- c(
      zlen = color2,
      zwei = color1,
      zbmi = color2,
      zwfl = color1
    )
    for (indicator in zscore_summary_cols) {
      res <- DT::formatStyle(res, zscore_create_col_labels(indicator),
        backgroundColor = color_map[indicator]
      )
    }
    res
  })
}
