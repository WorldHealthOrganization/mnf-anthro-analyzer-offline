PlotDigitOutput <- function(id) {
  ns <- NS(id)
  make_dl_button <- function(id) {
    shinyjs::disabled(downloadButton(ns(id), label = "Download figure"))
  }
  tabPanel(
    "Digit preference",
    icon = icon("sort-numeric-asc"),
    tags$style(type = "text/css", "#dl_digit { background-color:LightGrey; float:right; margin-right: 15px;}"),
    validatorOutput(ns("validation")),
    fluidRow(column(12, MappingSelectorOutput(ns("strat_variable")))),
    fluidRow(column(12, h5("Decimals' digit preference"))),
    fluidRow(column(12, make_dl_button("dl_digit"))),
    fluidRow(column(12, uiOutput(ns("dynamic_plot_digit")))),
    fluidRow(
      column(
        6,
        h5("Weight integer digit preference"),
        make_dl_button("dl_weight"),
        plotOutput(ns("integer_weight_plot"), height = "600px")
      ),
      column(
        6,
        h5("Length or height integer digit preference"),
        make_dl_button("dl_height"),
        plotOutput(ns("integer_height_plot"), height = "600px")
      )
    )
  )
}

PlotDigit <- function(input, output, session, list_map_vars, age_in_months,
                      dataset, original_filename) {
  ns <- session$ns

  validation <- callModule(validatorModule, "validation", reactive({
    check_validation_conditions(
      condition(
        nrow(dataset()) == 0L,
        validation_error(
          "The dataset does not have any rows."
        )
      ),
      condition(
        any(!c("age_group", "age_in_days") %in% colnames(dataset())),
        validation_error(
          "You have not provided a mapping for the age yet."
        )
      ),
      condition(
        !(list_map_vars[["weight"]]() != "None" && list_map_vars[["lenhei"]]() != "None"),
        validation_error(
          'Weight and "Length or Height" both need to be mapped to columns in your dataset.'
        )
      )
    )
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

  observe({
    req(validation())
    shinyjs::enable("dl_digit")
    shinyjs::enable("dl_weight")
    shinyjs::enable("dl_height")
  })

  # integer plots
  weight_plot <- reactive({
    req(validation())
    plot_data <- digit_prepare_weight_plot_data(
      dataset(),
      list_map_vars[["weight"]]()
    )
    digit_weight_plot(plot_data)
  })

  output$integer_weight_plot <- renderPlot({
    weight_plot()
  })

  height_plot <- reactive({
    plot_data <- digit_prepare_height_plot_data(
      dataset(),
      list_map_vars[["lenhei"]]()
    )
    digit_height_plot(plot_data)
  })

  output$integer_height_plot <- renderPlot({
    height_plot()
  })

  stratification_variable_selected <- reactive({
    stratification_variable <- selected_variable()
    length(stratification_variable$column) == 1L &&
      is_variable_mapped(stratification_variable$column) &&
      stratification_variable$column %in% colnames(dataset())
  })

  digit_preference <- reactive({
    req(validation())
    df_zscores <-
      CalculateZScores(
        data = dataset(),
        sex = list_map_vars[["sex"]](),
        weight = list_map_vars[["weight"]](),
        lenhei = list_map_vars[["lenhei"]](),
        lenhei_unit = list_map_vars[["lenhei_unit"]](),
        oedema = list_map_vars[["oedema"]]()
      )
    strat_column <- if (stratification_variable_selected()) {
      selected_variable()$column
    } else {
      NULL
    }
    digit_prepare_heaping_plot_data(df_zscores,
      weight_col = list_map_vars[["weight"]](),
      lenhei_col = list_map_vars[["lenhei"]](),
      strat_column = strat_column
    )
  })

  # compute the height of the facet digit plot dynamically to have enough space
  # for a larger name of levels. E.g. with teams.
  validate_heaping_plot <- function(n_levels) {
    validate(
      need(n_levels < 30, "This plot is only available for less than 30 groups")
    )
  }
  digit_heaping_plot_height <- reactive({
    if (stratification_variable_selected()) {
      req(is.data.frame(digit_preference()))
      n_levels <- dplyr::n_distinct(digit_preference()[["strat_var"]])
      validate_heaping_plot(n_levels)
      pmax(n_levels * 160, 600)
    } else {
      600
    }
  })

  digit_heaping_plot <-
    reactive({
      plot_data <- digit_preference()
      if (!is.null(plot_data[["strat_var"]])) {
        n_levels <- dplyr::n_distinct(plot_data[["strat_var"]])
        validate_heaping_plot(n_levels)
      }
      digit_create_heaping_plot(plot_data,
        is_stratification_variable_selected = stratification_variable_selected()
      )
    })

  output$plot_digit <- renderPlot({
    digit_heaping_plot()
  })

  output$dynamic_plot_digit <- renderUI({
    plotOutput(ns("plot_digit"), height = paste0(digit_heaping_plot_height(), "px"))
  })

  make_download_handler <- function(filename, plot_fun,
                                      export_fun = function(x) png(x, width = 800)) {
    downloadHandler(
      filename = function() make_export_filename(original_filename(), filename),
      content = function(file) {
        req(validation())
        export_fun(file)
        print(plot_fun())
        dev.off()
      }
    )
  }

  # Download plot
  output$dl_digit <- make_download_handler(
    "digit_heaping.png",
    digit_heaping_plot,
    function(x) png(x,
        width = 800,
        height = digit_heaping_plot_height()
      )
  )
  output$dl_height <- make_download_handler(
    "height_integer_histogram.png",
    height_plot
  )
  output$dl_weight <- make_download_handler(
    "weight_integer_histogram.png",
    weight_plot
  )
}
