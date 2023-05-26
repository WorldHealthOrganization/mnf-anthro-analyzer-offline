PlotDistOutput <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Distributions",
    icon = icon("area-chart"),
    validatorOutput(ns("validation")),
    fluidRow(
      column(4, VariablesInput(ns("id_variables_x"))),
      column(4, VariablesInput(ns("id_variables_strat_dist"))),
      column(4, shinyjs::hidden(downloadButton(ns("dl_plot"), label = "Download figure")))
    ),
    fluidRow(
      column(12, plotOutput(ns("plot_dist"))),
      column(12, uiOutput(ns("mismatch_table")))
    )
  )
}


PlotDist <- function(input, output, session, dataset, df_matched_vars_strat,
                     list_map_vars, original_filename) {
  ns <- session$ns

  validation <- callModule(validatorModule, "validation", reactive({
    if (any(!c(
      "age_group",
      "age_in_days",
      "age_in_months"
    ) %in% colnames(dataset()))) {
      validation_error(
        "You have not provided a mapping for the age yet."
      )
    } else {
      validation_ok()
    }
  }))

  plot_data <- reactive({
    df <- dataset()
    req(is.data.frame(df))
    req("age_in_months" %in% colnames(df))
    dist_prepare_plot_data(df)
  })

  variable_labels <- reactive({
    other_grouping_name <- anthro_make_grouping_label(list_map_vars[["othergr"]]())
    c(
      "Standard age group", "Age in years", "Age in months", "Weight (kg)", "Length or height (cm)", "LH measure", "Sex",
      "Residence\ntype", "Geographical\nregion",
      "Wealth\nquintile", "Mother\neducation", other_grouping_name
    )
  })

  variable_choices <- reactive({
    c(
      "age_group", "plot_age_in_years", "plot_age_in_months", list_map_vars[["weight"]](), list_map_vars[["lenhei"]](), list_map_vars[["lenhei_unit"]](),
      list_map_vars[["sex"]](), list_map_vars[["typeres"]](), list_map_vars[["gregion"]](),
      list_map_vars[["wealthq"]](), list_map_vars[["mothered"]](), list_map_vars[["othergr"]]()
    )
  })

  df_variable_names <-
    reactive(
      tibble::tibble(
        choice = variable_choices(),
        is_discrete = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
        name = variable_labels()
      )
    )

  x_choices <- reactive({
    req(validation())
    dataset_cols <- colnames(plot_data())
    choices <- variable_choices()
    available_choices <- choices %in% dataset_cols
    choices <- choices[available_choices]
    choices_labels <- variable_labels()[available_choices]
    setNames(choices, choices_labels)
  })

  var_x_discrete <- reactive({
    all(magrittr::extract2(
      dplyr::filter(df_variable_names(), choice == variable_x()),
      "is_discrete"
    ))
  })

  variable_x <- callModule(Variables, "id_variables_x",
    label = "Variable",
    choices = x_choices
  )

  observe({
    updateSelectInput(session, "id_variables_x", choices = x_choices())
  })

  strat_choices <- reactive({
    strat_tuples <- df_matched_vars_strat()
    selected_variable <- variable_x()

    # In case the user selects lenhei_unit, he/she should only be
    # able to stratify by team or geographical region.
    if (is.character(selected_variable) &&
      length(selected_variable) == 1L &&
      selected_variable != "None" &&
      selected_variable == list_map_vars[["lenhei_unit"]]()) {
      allowed_stratifications <- c(list_map_vars[["team"]](), list_map_vars[["gregion"]]())
      strat_tuples <- filter(strat_tuples, value %in% allowed_stratifications)
    }
    c("None", strat_tuples$key)
  })

  variable_strat_dist <-
    callModule(StratVariables, "id_variables_strat_dist",
      label = "Stratification variable",
      choices = strat_choices,
      selected = "None",
      show_element = validation
    )

  observe({
    if (validation()) {
      shinyjs::show("dl_plot")
    } else {
      shinyjs::hide("dl_plot")
    }
  })

  main_plot <- reactive({
    # Make sure requirements are met & hide error message if not
    tmp <- plot_data()
    req(is.data.frame(tmp))
    dataset_cols <- colnames(tmp)

    # validate assumptions
    var_x_name <- variable_x()
    req(var_x_name)
    req(is.character(var_x_name))
    req(length(var_x_name) == 1L)
    req(var_x_name %in% dataset_cols)
    req(length(variable_strat_dist()) == 1L)

    validate(
      need(
        var_x_name == "age_group" || var_x_name %in% dataset_cols,
        "The selected variable is not part of the dataset"
      )
    )

    selected_strat_var <-
      df_matched_vars_strat() %>%
      filter(key %in% variable_strat_dist()) %>%
      pull(value)

    get_var_label <- function(var_key) {
      magrittr::extract2(
        dplyr::filter(df_variable_names(), choice == var_key),
        "name"
      )
    }
    dist_plot(
      plot_data = tmp,
      x_variable = var_x_name,
      x_variable_label = get_var_label(var_x_name),
      is_x_discrete = var_x_discrete(),
      stratification_variable = selected_strat_var,
      stratification_variable_label = get_var_label(selected_strat_var)
    )
  })

  output$plot_dist <- renderPlot({
    main_plot()
  })

  # overview of LH measure mismatches
  show_mismatch_table <- reactive({
    length(list_map_vars[["lenhei_unit"]]()) == 1L &&
      length(variable_x()) &&
      is.character(list_map_vars[["lenhei_unit"]]()) &&
      is.character(variable_x()) &&
      variable_x() != "None" &&
      variable_x() == list_map_vars[["lenhei_unit"]]()
  })

  mismatch_table_data <- reactive({
    req(show_mismatch_table())
    req(all(c(
      "age_in_months",
      "age_group",
      list_map_vars[["lenhei_unit"]]()
    ) %in% colnames(plot_data())))
    measure_col <- as.symbol(list_map_vars[["lenhei_unit"]]())
    data <- dist_compute_mismatch_table(plot_data(), measure_col, as.symbol("age_in_months"))
    data
  })

  number_missing_values <- reactive({
    req(show_mismatch_table())
    req(validation())
    dist_mismatch_n_children(plot_data(), list_map_vars[["lenhei_unit"]]())
  })

  output$dl_mismatch_table <- downloadHandler(
    filename = function() make_export_filename(original_filename(), "mismatch_table.csv"),
    contentType = "text/csv",
    content = function(file) {
      mismatch_table_data() %>%
        mutate(`Age group` = gsub(
          x = `Age group`,
          pattern = "&nbsp;",
          replacement = " ",
          fixed = TRUE
        )) %>%
        readr::write_csv(file, na = "")
    }
  )

  output$mismatch_table <- renderUI({
    if (show_mismatch_table()) {
      list(
        downloadButton(
          ns("dl_mismatch_table"),
          label = "Download Mismatch table"
        ),
        h5("Mismatch table for LH measure"),
        br(),
        renderTable(mismatch_table_data(), sanitize.text.function = identity),
        p(paste0(
          "* Mismatch means children under 24 months were measured ",
          "standing (height) or children 24 months or older were measured ",
          "laying down (recumbent length), as opposed to the recommendation."
        )),
        p(paste0("Missing values: ", number_missing_values()))
      )
    } else {
      list()
    }
  })

  # Download plot
  output$dl_plot <- downloadHandler(
    filename = function() make_export_filename(original_filename(), "distributions.png"),
    content = function(file) {
      png(file, width = 800)
      print(main_plot())
      dev.off()
    }
  )
}
