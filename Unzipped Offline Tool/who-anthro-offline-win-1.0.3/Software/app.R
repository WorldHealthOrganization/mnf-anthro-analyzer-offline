suppressPackageStartupMessages({
  library(shiny, quietly = TRUE)
  library(shinyjs, quietly = TRUE)
  library(tidyr, quietly = TRUE)
  library(magrittr, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(forcats, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(lazyeval, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(purrr, quietly = TRUE)
  library(ggplot2, quietly = TRUE)
  library(survey, quietly = TRUE)
})

# source anthro standards
source("code/standards.R")

# source the auxiliary functions
source("code/functions-helper.R")
source("code/functions-z.R")
source("code/functions-prev.R")
source("code/data-assumptions.R")
source("code/input-variable-mapping.R")
source("code/plots.R")

# source the core functions
source("code/macro-z.R")
source("code/macro-prev.R")

# module support libraries
source("code/missing-data-lib.R")
source("code/distributions-lib.R")
source("code/zscore-dq-lib.R")
source("code/zscore-lib.R")
source("code/digit-lib.R")
source("code/data-quality-lib.R")

# source modules
source("modules/setup.R")
source("modules/upload.R")
source("modules/welcome.R")
source("modules/validator.R")
source("modules/mapping-selector.R")

## sidebar
source("modules/variables.R")
source("modules/variables-strat.R")

## main page
source("modules/datatable.R")
source("modules/zscore.R")
source("modules/prevalence.R")
source("modules/zscore-data-quality.R")
source("modules/data-quality.R")
source("modules/report.R")

## graphics
source("modules/plot-dist.R")
source("modules/plot-miss.R")
source("modules/plot-digit.R")

## ui --------------------------------------------------------------------------
source("modules/layout.R")
ui <- anthro_UI()

anthro_is_embedded <- isTRUE(Sys.getenv("ANTHRO_IS_EMBEDDED") == "1")

# sets upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)
if (anthro_is_embedded) {
  # unless it is executed locally then we allow 500MB
  options(shiny.maxRequestSize = 500 * 1024^2)
}

# options for prevalence
options("scipen" = 100, survey.lonely.psu = "adjust")

# check if test_mode is on
test_mode <- isTRUE(getOption("anthro.debug"))

## Variable mapping
df_map_vars <- anthro_input_variable_mapping()

## server ----------------------------------------------------------------------
server <- function(input, output, session) {
  # initial setup
  if (test_mode) {
    # this can be used for testing
    data_uploaded <- reactive({
      list(
        name = "testfile.csv",
        datapath = "../testdata/testfile.csv"
      )
    })
  } else {
    data_uploaded <- callModule(Upload, "tmp")
  }

  # data setup
  # upload data and return dataframes
  df_read <- reactive({
    inFile <- data_uploaded()
    req(!is.null(inFile))
    file_name <- inFile$name
    valid_filename <- grepl(
      x = tolower(file_name),
      pattern = "^[a-zA-Z0-9_\\-]{1,220}\\.csv$"
    )
    validate(
      need(
        valid_filename,
        'The uploaded file needs to be a csv and the file name can only contain characters, numbers, "_" and "-".'
      )
    )

    # detect encoding, this is probabilistic and may fail
    # suppress warnings to keep the logs clean of user generated warnings
    suppressWarnings({
      res <- anthro_read_uploaded_csv(inFile$datapath)
    })

    validate(
      need(
        is.data.frame(res),
        'You uploaded file is not a valid csv file with "," as delimiter.'
      ),
      need(
        nrow(res) > 0L,
        "Your dataset does not have any rows. Maybe the csv file is not formatted correctly?"
      ),
      need(
        ncol(res) > 1L,
        "Your dataset appears to only have one column. Maybe the csv file is not formatted correctly?"
      )
    )
    list(
      result = res,
      file_path = inFile$datapath,
      file_name = file_name
    )
  })

  original_filename <- reactive({
    file_name <- df_read()[["file_name"]]
    req(is.character(file_name))
    gsub(x = file_name, pattern = "\\.csv$", replacement = "", fixed = FALSE)
  })

  # this is the original data frame
  df_original <- reactive({
    req(is.list(df_read()))
    df <- df_read()[["result"]]
    req(is.data.frame(df))
    req(nrow(df) > 1L)
    req(ncol(df) > 1L)
    df
  })

  # this is the raw data frame after it was parsed
  # and NA rows are removed
  df_raw <- reactive({
    df <- df_original()
    df <- dplyr::filter(df, rowSums(is.na(df)) < ncol(df))
    req(nrow(df) > 1L)
    req(ncol(df) > 1L)
    df
  })

  num_removed_na_rows <- reactive({
    nrow(df_original()) - nrow(df_raw())
  })

  # delete the uploaded file from the temp files
  # as an additional security measure
  observe({
    if (!test_mode) {
      try({
        file.remove(df_read()[["file_path"]])
      }, silent = TRUE)
    }
  })

  ## Add age group when age variable gets mapped
  age_in_months <- reactive({
    if (!is.null(input$ui_setup_compute_age) ||
      is.null(input$compute_age)) {
      FALSE
    } else {
      input$id_age_unit && !isTRUE(input$compute_age)
    }
  })

  compute_age_by_dates <- reactive({
    !is.null(input$compute_age) && isTRUE(input$compute_age)
  })

  # this dataset is the raw dataset plus an age_group column
  df_age_group_base <- reactive({
    req(is.data.frame(df_raw()))
    req(nrow(df_raw()) >= 1L)
    req(length(list_map_vars) >= 1L)
    req(!compute_age_by_dates() ||
      length(list_map_vars[["date_birth"]]()) == 1L)
    req(!compute_age_by_dates() ||
      length(list_map_vars[["date_obs"]]()) == 1L)
    req(compute_age_by_dates() ||
      length(list_map_vars[["age"]]()) == 1L)

    # either age can be computed by date of birth/obs or by a column.
    # but never both
    dates_given <- list_map_vars[["date_birth"]]() != "None" &&
      list_map_vars[["date_obs"]]() != "None" &&
      compute_age_by_dates()
    age_given <-
      list_map_vars[["age"]]() != "None" && !compute_age_by_dates()
    month_scaling_constant <- 30.4375
    col_names <- colnames(df_raw())
    req(is.character(input$date_format) && length(input$date_format) == 1L)
    d_parse <- function(x) parse_date(x, format = input$date_format)
    tmp <- if (dates_given) {
      req(list_map_vars[["date_obs"]]() %in% col_names)
      req(list_map_vars[["date_birth"]]() %in% col_names)
      age_in_days <- as.integer(
        d_parse(df_raw()[[list_map_vars[["date_obs"]]()]]) -
        d_parse(df_raw()[[list_map_vars[["date_birth"]]()]])
      )
      mutate(
        df_raw(),
        age_in_days = as.integer(age_in_days),
        age_in_months = age_in_days / month_scaling_constant,
        age_group = anthro_age_groups(age_in_days / month_scaling_constant)
      )
    } else if (age_given) {
      req(list_map_vars[["age"]]() %in% col_names)
      age_values <- as.numeric(df_raw()[[list_map_vars[["age"]]()]])
      if (age_in_months()) {
        mutate(
          df_raw(),
          age_in_days = as.integer(round_up(age_values * month_scaling_constant)),
          age_in_months = age_values,
          age_group = anthro_age_groups(age_in_months)
        )
      } else {
        mutate(
          df_raw(),
          age_in_days = as.integer(age_values),
          age_in_months = age_values / month_scaling_constant,
          age_group = anthro_age_groups(age_in_months)
        )
      }
    } else {
      df_raw()
    }

    for (col in c(
      list_map_vars[["date_obs"]](),
      list_map_vars[["date_birth"]]()
    )) {
      if (!col %in% "None" && col %in% col_names) {
        tmp[[col]] <- d_parse(tmp[[col]])
      }
    }

    for (col in c(
      list_map_vars[["age"]](),
      list_map_vars[["weight"]](),
      list_map_vars[["lenhei"]](),
      list_map_vars[["sw"]](),
      list_map_vars[["cluster"]](),
      list_map_vars[["strata"]]()
    )) {
      if (!col %in% "None" && col %in% col_names) {
        tmp[[col]] <- as.numeric(tmp[[col]])
      }
    }
    mutate(tmp, uid = row_number())
  })


  df_age_group <- reactive({
    req(is.data.frame(df_age_group_base()))

    # here we exclude all children with age >= 60 monhts
    if ("age_in_months" %in% colnames(df_age_group_base())) {
      dplyr::filter(df_age_group_base(), is.na(age_in_months) | age_in_months < 60)
    } else {
      df_age_group_base()
    }
  })

  number_removed_due_to_age <- reactive({
    req(is.data.frame(df_age_group_base()))
    req(is.data.frame(df_age_group()))

    nrow(df_age_group_base()) - nrow(df_age_group())
  })

  # the last dataset is the dataset where filters might have been applied to
  in_memory_database <- reactiveValues()

  # set the main data frame to an intial values
  observe({
    req(is.data.frame(df_age_group()))
    in_memory_database[["df_filtered"]] <- df_age_group()
  })

  df_filtered <- reactive({
    df <- in_memory_database[["df_filtered"]]
    req(is.data.frame(df))
    df
  })

  ####################################
  ##### Code for dynamic filters #####
  ####################################

  ## Call main filter module
  ## only have filters with less than 100 unique values
  filter_choices <- reactive({
    purrr::keep(
      colnames(df_raw()),
      ~dplyr::between(dplyr::n_distinct(df_raw()[[.x]]), 2L, 30L)
    )
  })

  filter_variables <-
    callModule(
      Variables,
      "id_variables_filter",
      label = "Filter variables",
      choices = filter_choices,
      selected = "",
      multiple = TRUE
    )

  # this list of reactive values does the bookkeeping of input values
  in_memory_database[["selected_filter_values"]] <- list()

  observeEvent(df_read(), {
    in_memory_database[["selected_filter_values"]] <- list()
  })


  # this renders the global filters box
  output$ui_filter <- renderUI({
    # first we read the filters from the filter select box
    filters <- filter_variables()

    # in this slot we store the current selected value for each filter
    # this needs to happen as we have to redraw the UI elements whenever the filters are changed
    in_memory_database[["selected_filter_values"]] <-
      isolate(in_memory_database[["selected_filter_values"]][filters])

    # this stores the current filters; this is used in get_current_dataset() and triggers a reload
    in_memory_database[["current_filters"]] <- filters

    lapply(filters, function(name) {
      req(name %in% colnames(df_raw()))
      control_name <- paste0("filter_", name)
      row_values <- df_age_group()[[name]]
      selected_value <-
        isolate(in_memory_database[["selected_filter_values"]][[name]])
      choices <- sort(unique(as.character(df_age_group()[[name]])))
      selectInput(
        control_name,
        paste0("Values: ", name),
        choices,
        multiple = TRUE,
        selected = selected_value
      )
    })
  })


  # Whenever filter is selected, this redraws the select boxes, keeping previous entries in place
  observe({
    # one observer per filter element
    filters <- filter_variables()

    # for each of the previous filters we had an observer
    # we first need to make sure these observers are being destroyed before creating new ones
    if (!is.null(isolate(in_memory_database[["filters_update_observers"]]))) {
      isolate(in_memory_database[["filters_update_observers"]]) %>% lapply(function(x)
        x$destroy())
    }

    # for each filter we create a new observer that takes care of the selected value bookkeeping
    in_memory_database[["filters_update_observers"]] <-
      filters %>%
      lapply(function(f) {
        element_name <- paste0("filter_", f)
        observe({
          in_memory_database[["selected_filter_values"]][[f]] <-
            input[[element_name]]
          in_memory_database[["filters_value_changed"]] <-
            Sys.time()
        })
      })
  })

  # fires if the button was clicked or a new age group is defined
  observeEvent({
    input$apply_filter
    df_age_group()
  }, {
    res <- if (is.null(in_memory_database[["current_filters"]])) {
      df_age_group()
    } else {
      Reduce(
        f = function(dataset, filter_name) {
          filter_vals <- input[[paste0("filter_", filter_name)]]
          if (length(filter_vals) == 0L || is.null(filter_vals) ||
            any(is.na(filter_vals)) || all(filter_vals == "")) {
            return(dataset)
          }
          req(filter_name %in% colnames(df_raw()))
          col_name <- as.name(filter_name)
          dplyr::filter(dataset, (!!col_name) %in% filter_vals)
        },
        init = df_age_group(),
        x = in_memory_database[["current_filters"]]
      )
    }
    in_memory_database[["df_filtered"]] <- res
  })

  output$panel_side <- renderUI({
    req(is.list(df_read()))
    req(is.data.frame(df_raw()))
    req(ncol(df_raw()) > 1L)
    req(nrow(df_raw()) >= 1L)
    ui_setup()[["side"]]
  })

  output$panel_main <- renderUI({
    if (is.null(data_uploaded())) {
      callModule(Welcome, "tmp")
    } else {
      ui_setup()[["main"]]
    }
  })

  observeEvent(input$close, {
    js$closeWindow()
    stop()
  })

  # event for the front page
  observeEvent(input$gotoanthro, {
    updateTabsetPanel(session, "mainNavBar",
                      selected = "analyser")
  })


  # this is a list of reactive values having the column mapping
  list_map_vars <- map(1:nrow(df_map_vars), function(i) {
    module_ns <- paste0("id_variables_", df_map_vars$vars[i])
    validator_explanation <- df_map_vars$validator_explanations[[i]]
    choices <- reactive({
      validator <- df_map_vars$validators[[i]]
      valid_df_names <- keep(names(df_raw()), function(col_name) {
        validator(df_raw()[[col_name]])
      })
      if (length(valid_df_names) == 0L) {
        c("Unavailable" = "None")
      } else {
        c("None", valid_df_names)
      }
    })
    popover_title <- reactive({
      if (length(choices()) == 1L && choices() == "None") {
        "No variables with correct format in dataset"
      }
    })
    popover_content <- reactive({
      if (length(choices()) == 1L && choices() == "None") {
        paste0(
          "We could not identify any columns in your dataset that match the formatting criteria for this variable. ",
          validator_explanation
        )
      }
    })
    callModule(
      Variables,
      module_ns,
      choices = choices,
      label = df_map_vars$labs[i],
      selected = "None",
      popover_title = popover_title,
      popover_content = popover_content
    )
  }) %>%
    set_names(df_map_vars$vars)


  matched_vars <- reactive({
    plyr::ldply(
      df_map_vars$vars,
      function(i)
        list_map_vars[[i]]()
    ) %>%
      filter(V1 != "None") %>%
      extract2("V1")
  })

  ## Variable mapping for stratification
  matched_vars_strat <- reactive({
    plyr::ldply(
      c(
        "sex",
        "typeres",
        "gregion",
        "wealthq",
        "mothered",
        "team",
        "othergr"
      ),
      function(i)
        list_map_vars[[i]]()
    ) %>%
      filter(V1 != "None") %>%
      extract2("V1")
  })

  df_matched_vars_strat <- reactive({
    tibble(
      key = c(
        "Age group (months)",
        "Sex",
        "Residence type",
        "Geographical region",
        "Wealth quintile",
        "Mother education",
        "Team",
        "Other grouping variable"
      ),
      value = c(
        "age_group",
        list_map_vars[["sex"]](),
        list_map_vars[["typeres"]](),
        list_map_vars[["gregion"]](),
        list_map_vars[["wealthq"]](),
        list_map_vars[["mothered"]](),
        list_map_vars[["team"]](),
        list_map_vars[["othergr"]]()
      )
    ) %>%
      filter(!value %in% "None")
  })

  # ui setup
  ui_setup <- callModule(
    SetUp,
    "ui_setup",
    tabsetPanel(
      DFTableOutput("DFTable"),
      zscoreOutput("zscore"),
      PrevalenceOutput("Prevalence"),
      DataQualityModuleOutput("DataQuality"),
      ReportOutput("id_report")
    ),
    original_filename
  )

  # ui Modules that depend on other reactive elements
  callModule(DFTable, "DFTable",
    dataset = df_filtered,
    num_removed_na_rows = num_removed_na_rows
  )
  callModule(
    zscore,
    "zscore",
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    dataset = df_filtered,
    df_matched_vars_strat,
    df_age_group = df_age_group_base,
    original_filename = original_filename
  )
  callModule(
    Prevalence,
    "Prevalence",
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    dataset = df_filtered,
    original_filename = original_filename
  )
  callModule(
    DataQualityModule,
    "DataQuality",
    df_matched_vars_strat = df_matched_vars_strat,
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    df_filtered = df_filtered,
    original_filename = original_filename
  )
  callModule(
    Report,
    "id_report",
    df_raw = df_raw,
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    df_filtered = df_filtered,
    original_filename = original_filename,
    number_removed_due_to_age = number_removed_due_to_age
  )

  if (Sys.getenv("ANTHRO_IS_EMBEDDED") == "1") {
    # in case it is embedded, we kill the app when the session ends
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  if (!interactive() && Sys.getenv("ANTHRO_IS_EMBEDDED") != "1") {
    showModal(modalDialog(
      title = "Disclaimer",
      HTML(
        "<p>The Anthro Survey Analyser runs in its own protected environment and access is SSL encrypted, and uploaded data are not saved once you close the session. However, the data will be temporarily stored in the cloud hosting the application and thus, users are advised to ensure data is de-identifiable.</p>
<p>The cloud provider, shinyapps.io, is currently hosted on Amazon’s Web Services (AWS) infrastructure in the us-east-1 region. Note that the infrastructure is not HIPAA-compliant.</p>
<p>After 15 minutes of idle time the app terminates the session and deletes all data.</p>
<p>All reasonable precautions have been taken by WHO to verify the calculations performed by this application. However, the application is being distributed without warranty of any kind, either express or implied. The responsibility for the use and interpretation of the application’s output lies with the user. In no event shall the World Health Organization be liable for damages arising from its use.</p>"
      ),
      footer = tagList(
        modalButton("Accept", icon = icon("check")),
        extendShinyjs(
          text = "shinyjs.closeWindow = function() { window.close(); }",
          functions = c("closeWindow")
        ),
        actionButton(
          "close",
          "Cancel",
          icon = icon("remove"),
          style = "color: #ED4337;"
        )
      )
    ))
  }
}

shinyApp(ui, server)
