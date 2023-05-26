generate_dq_report_variables <- function(df_filtered, list_map_vars) {
  df_filtered <- df_filtered()
  list_map_vars <- list_mapping_to_char(list_map_vars)
  n_children <- nrow(df_filtered)
  df_zscores <- CalculateZScores(
    data = df_filtered,
    sex = list_map_vars[["sex"]],
    weight = list_map_vars[["weight"]],
    lenhei = list_map_vars[["lenhei"]],
    lenhei_unit = list_map_vars[["lenhei_unit"]],
    oedema = list_map_vars[["oedema"]]
  )
  is_geo_mapped <- is_variable_mapped(list_map_vars[["gregion"]])
  is_team_mapped <- is_variable_mapped(list_map_vars[["team"]])
  is_wealthq_mapped <- is_variable_mapped(list_map_vars[["wealthq"]])
  is_measure_mapped <- is_variable_mapped(list_map_vars[["lenhei_unit"]])
  missing_plot_data <- function(stratify_by = NULL) {
    missing_data_prepare_plot_data(
      dataset = df_filtered,
      stratification_variable = stratify_by,
      is_stratification_variable_selected = !is.null(stratify_by),
      othergr_col = list_map_vars[["othergr"]],
      sex_col = list_map_vars[["sex"]],
      typeres_col = list_map_vars[["typeres"]],
      gregion_col = list_map_vars[["gregion"]],
      wealthq_col = list_map_vars[["wealthq"]],
      mothered_col = list_map_vars[["mothered"]],
      weight_col = list_map_vars[["weight"]],
      lenhei_col = list_map_vars[["lenhei"]],
      lenhei_unit_col = list_map_vars[["lenhei_unit"]],
      oedema_col = list_map_vars[["oedema"]]
    )
  }
  digit_grouped_heaping_plot <- function(strat_column) {
    plot_data <- digit_prepare_heaping_plot_data(
      df_zscores,
      weight_col = list_map_vars[["weight"]],
      lenhei_col = list_map_vars[["lenhei"]],
      strat_column = strat_column
    )
    strata <- sort(unique(plot_data[["strat_var"]]))
    n_strata <- length(strata)
    subplots <- 3L # the number of rows per facet plot
    n_plots <- as.integer(n_strata / subplots)
    if (n_strata %% subplots > 0) {
      n_plots <- n_plots + 1L
    }
    if (n_plots < 1 || n_plots > 50) { # for safety
      return()
    }
    for (i in 1:n_plots) {
      strata_plot <- strata[seq_len(pmin(subplots, length(strata)))]
      strata <- setdiff(strata, strata_plot)
      data <- dplyr::filter(plot_data, strat_var %in% strata_plot)
      if (nrow(data) > 0) {
        print(digit_create_heaping_plot(data, TRUE))
      }
    }
  }
  list(
    total_number_of_children = function() {
      n_children
    },
    missing_data_plot = function() {
      missing_data_make_plot(missing_plot_data())
    },
    show_missing_data_by_geo = is_geo_mapped,
    show_missing_data_by_team = is_team_mapped,
    missing_data_table_by_geo = function() {
      label <- "Geographical region"
      missing_data_prepare_missings_table(
        missing_plot_data(stratify_by = list(label = label, column = list_map_vars[["gregion"]])),
        label
      )
    },
    missing_data_table_by_team = function() {
      label <- "Team"
      missing_data_prepare_missings_table(
        missing_plot_data(stratify_by = list(label = label, column = list_map_vars[["team"]])),
        label
      )
    },
    dist_age_group_by_sex = function() {
      dist_plot(
        plot_data = dist_prepare_plot_data(df_filtered),
        x_variable = "age_group",
        x_variable_label = "Standard age group",
        is_x_discrete = TRUE,
        stratification_variable = list_map_vars[["sex"]],
        stratification_variable_label = "Sex"
      )
    },
    dist_age_year_by_sex = function() {
      dist_plot(
        plot_data = dist_prepare_plot_data(df_filtered),
        x_variable = "plot_age_in_years",
        x_variable_label = "Age in years",
        is_x_discrete = TRUE,
        stratification_variable = list_map_vars[["sex"]],
        stratification_variable_label = "Sex"
      )
    },
    show_dist_age_year_by_wealthq = is_wealthq_mapped,
    dist_age_year_by_wealthq = function() {
      dist_plot(
        plot_data = dist_prepare_plot_data(df_filtered),
        x_variable = list_map_vars[["lenhei"]],
        x_variable_label = "Length or height (cm)",
        is_x_discrete = FALSE,
        stratification_variable = list_map_vars[["wealthq"]],
        stratification_variable_label = "Wealth quintile"
      )
    },
    show_mismatch_table = is_measure_mapped,
    mismatch_table = function() {
      dist_compute_mismatch_table(
        data = dist_prepare_plot_data(df_zscores),
        measure_col = as.symbol(list_map_vars[["lenhei_unit"]]),
        age_in_month_col = as.symbol("age_in_months")
      )
    },
    mismatch_n_children = function() {
      dist_mismatch_n_children(
        dist_prepare_plot_data(df_zscores),
        list_map_vars[["lenhei_unit"]]
      )
    },
    zscore_plot_flagged_zscores = function() {
      zscore_plot_flagged_zscores(df_zscores)
    },
    zscore_plot_dist = function() {
      data <- zscore_prepare_plot_dist_data(
        df_zscores = df_zscores,
        stratification_var_name = "None",
        list_map_vars = list_map_vars
      )
      zscore_plot_distribution(data)
    },
    zscore_plot_dist_by_sex = function() {
      data <- zscore_prepare_plot_dist_data(
        df_zscores = df_zscores,
        stratification_var_name = "Sex",
        list_map_vars = list_map_vars
      )
      zscore_plot_distribution(data)
    },
    zscore_plot_dist_by_age_group = function() {
      data <- zscore_prepare_plot_dist_data(
        df_zscores = df_zscores,
        stratification_var_name = "Age group (months)",
        list_map_vars = list_map_vars
      )
      zscore_plot_distribution(data)
    },
    zscore_summary_table = function() {
      data <- zscore_compute_summary_data(
        set_flagged_zscores_to_na(df_zscores),
        zscore_summary_groups(list_map_vars)
      )
      # for the report we split the data frame into two tables
      stopifnot(ncol(data) == 18)
      df1 <- data[, 1:10]
      df2 <- dplyr::bind_cols(data[, 1:2], data[, 11:18])
      list(
        df1 = df1,
        df2 = df2
      )
    },
    digit_plot_heaping = function() {
      plot_data <- digit_prepare_heaping_plot_data(
        df_zscores,
        weight_col = list_map_vars[["weight"]],
        lenhei_col = list_map_vars[["lenhei"]],
        strat_column = NULL
      )
      digit_create_heaping_plot(
        plot_data,
        is_stratification_variable_selected = FALSE
      )
    },
    show_digit_plot_heaping_by_team = is_team_mapped,
    digit_plot_heaping_by_team = function() {
      digit_grouped_heaping_plot(list_map_vars[["team"]])
    },
    show_digit_plot_heaping_by_region = is_geo_mapped,
    digit_plot_heaping_by_region = function() {
      digit_grouped_heaping_plot(list_map_vars[["gregion"]])
    },
    digit_plot_integer_for_weight = function() {
      plot_data <- digit_prepare_weight_plot_data(
        df_filtered,
        list_map_vars[["weight"]]
      )
      digit_weight_plot(plot_data)
    },
    digit_plot_integer_for_lenhei = function() {
      plot_data <- digit_prepare_height_plot_data(
        df_filtered,
        list_map_vars[["lenhei"]]
      )
      digit_height_plot(plot_data)
    }
  )
}

generate_dq_report <- function(variables, file) {
  output_format <- rmarkdown::word_document(
    reference_docx = "dq-report-template.docx",
    toc = TRUE,
    toc_depth = 2,
    fig_width = 10,
    fig_height = 7,
    keep_md = FALSE
  )
  envir <- rlang::as_environment(variables, globalenv())
  suppressMessages(
    suppressWarnings(
      rmarkdown::render(
        input = "reports/dq-report.Rmd",
        output_format = output_format,
        output_file = file,
        intermediates_dir = file.path(tempdir(), digest::sha1(runif(1))),
        envir = envir,
        quiet = TRUE
      )
    )
  )
}
