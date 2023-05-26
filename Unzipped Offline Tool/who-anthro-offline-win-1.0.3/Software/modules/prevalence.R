PrevalenceOutput <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Prevalence",
    icon = icon("bar-chart"),

    tags$style(type = "text/css", "#dl_prevalence {background-color:LightGrey; float:right; margin-bottom: 15px;}"),
    validatorOutput(ns("validation")),
    uiOutput(ns("warnings")),
    uiOutput(ns("nested_clusters")),
    fluidRow(
      column(
        12,
        shinyjs::disabled(actionButton(
          ns("button_prev"),
          label = "Click to calculate prevalence estimates"
        )),

        shinyjs::disabled(
          downloadButton(
            ns("dl_prevalence"),
            label = "Download prevalence estimates"
          )
        )
      )
    ),
    fluidRow(column(12, DT::dataTableOutput(ns("prev"))))
  )
}

Prevalence <- function(input, output, session, list_map_vars, age_in_months,
                       dataset, original_filename) {
  ns <- session$ns

  validation_checks <- callModule(
    validatorModule,
    "validation",
    anthro_create_prevalence_validation(list_map_vars, dataset)
  )

  clusters_not_nested <- reactive({
    are_there_non_nested_clusters(dataset, list_map_vars)
  })

  cluster_nesting_ok <- reactive({
    # if clusters are not nested the user needs to force nesting
    !clusters_not_nested() || isTRUE(input$force_nesting)
  })

  validation <- reactive({
    isTRUE(validation_checks()) && cluster_nesting_ok()
  })

  output$nested_clusters <- renderUI({
    nested_clusters <- !clusters_not_nested()
    if (isFALSE(validation_checks()) || nested_clusters) {
      return(list())
    }
    list(
      br(),
      div(
        class = "alert alert-warning",
        h4("Cluster ids not nested within strata",
           class = "alert-heading"
        ),
        list(
          p(paste0("Note that the clusters are not currently nested within strata; that is, cluster numbers overlap across strata.",
                   " If that should not be the case, you can fix the incorrect labels in your dataset and re-upload it. ",
                   "Otherwise, just tick \"Continue\" and the tool will relabel the clusters to enforce nesting within strata and calculate prevalence estimates.")),
          shiny::checkboxInput(ns("force_nesting"),
                               label = "Continue",
                               width = "100%"
          )
        )
      )
    )
  })

  output$warnings <- renderUI({
    req(validation())

    missing_vals <- anthro_prev_excluded_cases(dataset(), list_map_vars)
    missing_values_names <- missing_vals$missing_values_names
    missing_value_counts <- missing_vals$missing_value_counts

    if (length(missing_values_names) > 0L) {
      error_div <- div(
        class = "alert alert-warning",
        h4("The following columns include missing values", class = "alert-heading"),
        tags$ul(map2(missing_values_names, missing_value_counts, function(field, count) {
          tags$li("'", field, "' with ", count, " missing ", if (count == 1) "case" else "cases")
        })),
        hr(),
        p("All rows with missing values in these columns will be excluded from the analysis!")
      )
      fluidRow(column(12, error_div))
    } else {
      HTML("")
    }
  })

  validated_dataset <- reactive({
    req(validation())
    dataset()
  })

  output$nrows <- renderText(nrow(validated_dataset()))

  observe({
    if (validation()) {
      shinyjs::enable("button_prev")
    } else {
      shinyjs::disable("button_prev")
    }
  })

  prev_db <- reactiveValues()
  observeEvent(dataset(), {
    prev_db[["df_prevs"]] <- NULL
  })

  # Run prevalence calculations
  ## Only run calculations once button explicitly pressed
  observeEvent(input$button_prev, {
    req(validation())
    inc_progress <- function() {
      incProgress(0.1)
    }
    withProgress(
      message = "Calculation in progress",
      detail = "Please be patient - This may take a while...",
      value = 0.1, {
        df_prevs <- try(
          CalculateZScores(
            data = validated_dataset(),
            sex = list_map_vars[["sex"]](),
            weight = list_map_vars[["weight"]](),
            lenhei = list_map_vars[["lenhei"]](),
            lenhei_unit = list_map_vars[["lenhei_unit"]](),
            oedema = list_map_vars[["oedema"]]()
          ) %>%
            CalculatePrev(
              data = .,
              age.month = age_in_months(), age = list_map_vars[["age"]](),
              date_birth = list_map_vars[["date_birth"]](), date_obs = list_map_vars[["date_obs"]](),
              sex = list_map_vars[["sex"]](),
              weight = list_map_vars[["weight"]](), lenhei = list_map_vars[["lenhei"]](),
              lenhei_unit = list_map_vars[["lenhei_unit"]](), sw = list_map_vars[["sw"]](),
              cluster = list_map_vars[["cluster"]](), strata = list_map_vars[["strata"]](),
              typeres = list_map_vars[["typeres"]](), gregion = list_map_vars[["gregion"]](),
              wealthq = list_map_vars[["wealthq"]](), mothered = list_map_vars[["mothered"]](),
              othergr = list_map_vars[["othergr"]](),
              oedema = list_map_vars[["oedema"]](),
              on_progress = inc_progress
            )
        )
        incProgress(0.8)
        prev_db[["df_prevs"]] <- df_prevs
      }
    )
  })

  output$prev <- DT::renderDataTable({
    req(validation())
    shiny::validate(
      need(!inherits(prev_db[["df_prevs"]], "try-error"),
           "Something unexpected went wrong during the computation. Please contact the administrator of this application.")
    )
    req(is.data.frame(prev_db[["df_prevs"]]))
    data <- dplyr::mutate_at(prev_db[["df_prevs"]], dplyr::vars(dplyr::ends_with("_se")), dplyr::funs(round(., digits = 4)))
    for (col in colnames(data)) {
      is_unweighted_pop <- grepl(x = col, pattern = "\\_unwpop$")
      is_weighted_pop <- grepl(x = col, pattern = "\\_pop$")
      is_not_se <- !grepl(x = col, pattern = "\\_se$")
      if (is_unweighted_pop) {
        data[[col]] <- as.integer(data[[col]])
      } else if (is_weighted_pop) {
        data[[col]] <- round(as.numeric(data[[col]]), digits = 1)
      } else if (is_not_se && is.numeric(data[[col]])) {
        data[[col]] <- round(data[[col]], digits = 2)
      }
    }
    data
  }, options = list(paging = FALSE, digits = 2))


  # Download table
  ## Only enable download button once z-score calculation button has been clicked
  observeEvent(input$button_prev, {
    req(validation())
    shinyjs::enable("dl_prevalence")
  })

  output$dl_prevalence <- downloadHandler(
    filename = function() {
      req(validation())
      make_export_filename(original_filename(), "prevalence.csv")
    },
    content = function(file) {
      req(validation())
      readr::write_csv(prev_db[["df_prevs"]], file, na = "")
    }
  )
}
