PlotMissOutput <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Missing data",
    icon = icon("bar-chart"),
    tags$style(type = "text/css", "#dl_miss { background-color:LightGrey; float:right; margin-right: 15px;}"),
    validatorOutput(ns("validation")),
    fluidRow(
      column(12, MappingSelectorOutput(ns("strat_variable")))
    ),
    fluidRow(
      column(12, uiOutput(ns("missing_plot_table")))
    ),
    fluidRow(column(12, uiOutput(ns("negative_age_sentence"))))
  )
}

PlotMiss <- function(input, output, session, list_map_vars, age_in_months,
                     dataset, original_filename) {
  ns <- session$ns

  validation <- callModule(validatorModule, "validation", reactive({
    if (any(!c("age_group", "age_in_days") %in% colnames(dataset()))) {
      validation_error(
        "You have not provided a mapping for the age yet."
      )
    } else {
      validation_ok()
    }
  }))

  selected_variable <- callModule(MappingSelector,
    id = "strat_variable",
    label = "Stratification variable",
    available_choices = c(
      "Team" = "team",
      "Geographical region" = "gregion"
    ),
    show_select_box = validation,
    list_map_vars = list_map_vars
  )

  stratification_variable_selected <- reactive({
    stratification_variable <- selected_variable()
    length(stratification_variable$column) == 1L &&
      is_variable_mapped(stratification_variable$column) &&
      stratification_variable$column %in% colnames(dataset())
  })

  plot_data <- reactive({
    req(validation())
    req(length(age_in_months()) == 1L)
    req(nrow(dataset()) >= 1L)
    req(all(
      vapply(names(list_map_vars), function(name) {
        # age can also be null
        x <- list_map_vars[[name]]
        (name == "age" && is.null(x())) ||
          (length(x()) == 1L &&
            is.character(x()) &&
            (x() == "None" || x() %in% colnames(dataset())))
      }, logical(1L))
    ))
    missing_data_prepare_plot_data(
      dataset = dataset(),
      stratification_variable = selected_variable(),
      is_stratification_variable_selected = stratification_variable_selected(),
      othergr_col = list_map_vars[["othergr"]](),
      sex_col = list_map_vars[["sex"]](),
      typeres_col = list_map_vars[["typeres"]](),
      gregion_col = list_map_vars[["gregion"]](),
      wealthq_col = list_map_vars[["wealthq"]](),
      mothered_col = list_map_vars[["mothered"]](),
      weight_col = list_map_vars[["weight"]](),
      lenhei_col = list_map_vars[["lenhei"]](),
      lenhei_unit_col = list_map_vars[["lenhei_unit"]](),
      oedema_col = list_map_vars[["oedema"]]()
    )
  })

  select_variable_label <- reactive({
    req(stratification_variable_selected())
    selected_variable()$label
  })

  display_plot <- reactive({
    is.data.frame(plot_data()) && isFALSE(stratification_variable_selected())
  })

  display_table <- reactive({
    is.data.frame(plot_data()) && isTRUE(stratification_variable_selected())
  })

  output$validation <- renderText({
    validation()
    ""
  })

  missing_data_plot <- reactive({
    req(display_plot())
    missing_data_make_plot(plot_data())
  })

  # this shows the missing values as a table
  # instead of a stratified plot
  missing_data_table <- reactive({
    req(display_table())
    missing_data_prepare_missings_table(
      plot_data(),
      select_variable_label()
    )
  })

  output$missing_data_plot <- renderPlot({
    missing_data_plot()
  })

  output$missing_data_table <- DT::renderDT({
    missing_data_table()
  },
  selection = "none", rownames = FALSE,
  options = list(
    paging = FALSE,
    digits = 2,
    bFilter = FALSE
  )
  )

  output$dl_miss_table <- downloadHandler(
    filename = function() make_export_filename(original_filename(), "missing_data.csv"),
    content = function(file) {
      readr::write_csv(missing_data_table(), file, na = "")
    }
  )

  output$missing_plot_table <- renderUI({
    req(display_plot() || display_table())
    if (display_plot()) {
      list(
        downloadButton(ns("dl_miss"), label = "Download figure"),
        plotOutput(ns("missing_data_plot"))
      )
    } else {
      list(
        downloadButton(ns("dl_miss_table"), label = "Download table"),
        DT::DTOutput(ns("missing_data_table")),
        p(tags$sub(MISSING_DATA_AGE_FOOTNOTE))
      )
    }
  })

  output$negative_age_sentence <- renderUI({
    req("age_in_days" %in% colnames(dataset()))
    negative_age <- count_negative_age(dataset()[["age_in_days"]])
    if (negative_age > 0) {
      was <- if (negative_age == 1) "was" else "were"
      child <- if (negative_age == 1) "child" else "children"
      percent <- format(round(negative_age / nrow(dataset()) * 100, 1L), digits = 1L)
      tags$p("There ", was, " ", negative_age, " (", percent, "%) ", child, " with negative age.")
    } else {
      list()
    }
  })

  # Download plot
  output$dl_miss <- downloadHandler(
    filename = function() make_export_filename(original_filename(), "missing_data.png"),
    content = function(file) {
      png(file, width = 800)
      print(missing_data_plot())
      dev.off()
    }
  )
}
