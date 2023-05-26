# Functions used for the missing data module
# Mostly extracted from the module
missing_data_prepare_plot_data <- function(dataset,
                                           stratification_variable,
                                           is_stratification_variable_selected,
                                           othergr_col,
                                           sex_col,
                                           typeres_col,
                                           gregion_col,
                                           wealthq_col,
                                           mothered_col,
                                           weight_col,
                                           lenhei_col,
                                           lenhei_unit_col,
                                           oedema_col) {
  other_grouping_name <- anthro_make_grouping_label(othergr_col)
  df_strat <-
    tibble(
      key = c(
        "Age group", "Sex", "Residence type", "Geographical region",
        "Wealth quintile", "Mother education", other_grouping_name
      ),
      value = c(
        "age_group", sex_col, typeres_col,
        gregion_col, wealthq_col,
        mothered_col, othergr_col
      )
    ) %>%
    filter(!value %in% "None")
  quality_vars <-
    tibble(
      key = c("Age* (days)", "Weight (kg)", "Length or height (cm)"),
      value = c("age_in_days", weight_col, lenhei_col)
    ) %>%
    bind_rows(df_strat) %>%
    mutate(
      key = fct_inorder(key)
    ) %>%
    filter(!value %in% c("None", "age_group"))
  req(nrow(quality_vars) >= 1L)
  df_zscores <-
    CalculateZScores(
      data = dataset,
      sex = sex_col,
      weight = weight_col,
      lenhei = lenhei_col,
      lenhei_unit = lenhei_unit_col,
      oedema = oedema_col
    )
  strat <- character(0L)
  if (is_stratification_variable_selected) {
    strat <- stratification_variable$column
  }
  df_missing <-
    count_missing_values(df_zscores, setdiff(quality_vars$value, strat), strat) %>%
    left_join(quality_vars, by = c("var" = "value")) %>%
    rename(name = var) %>%
    mutate(
      name = fct_relevel(name, unique(quality_vars$value)),
      value = round(na / n * 100, 1L),
      value_absolute = na,
      missing = str_c(na, " (", value, "%)")
    ) %>%
    arrange(name) %>%
    select(key, everything())
  df_missing
}

missing_data_prepare_missings_table <- function(plot_data,
                                                stratification_variable_label) {
  res <- plot_data %>%
    select(-na, -value, -value_absolute, -name) %>%
    tidyr::spread(key, value = "missing")
  colnames(res) <- c(stratification_variable_label, "N", colnames(res)[-1:-2])
  res
}


count_negative_age <- function(ages) {
  sum(ages < 0, na.rm = TRUE)
}

# counts missing values and is used to calculate the results for the plots
count_missing_values <- function(data, columns, stratifications) {
  data <- dplyr::select(data, dplyr::one_of(c(columns, stratifications)))
  strat_symbols <- lapply(stratifications, as.symbol)
  data <- dplyr::group_by(data, !!!strat_symbols)
  data <- dplyr::summarise_all(data, list(na = ~sum(is.na(.)), n = ~dplyr::n()))
  if (length(columns) == 1L) {
    # in this case we prefix columns na and n by the column name so that
    # the output is equal to the length(columns) > 1 case
    data[[paste0(columns, "_n")]] <- data[["n"]]
    data[[paste0(columns, "_na")]] <- data[["na"]]
    data[["na"]] <- NULL
    data[["n"]] <- NULL
  }
  data <- tidyr::gather(data, name, value, -dplyr::one_of(stratifications))
  split_extract <- function(str, pos) {
    vapply(strsplit(str, "_", fixed = TRUE), function(x) {
      paste0(x[pos], collapse = "_")
    }, character(1L))
  }
  count_underscores <- function(str) {
    vapply(strsplit(str, "_", fixed = TRUE), length, integer(1L))
  }
  rm_last <- function(x) {
    n <- length(x)
    x[-n]
  }
  data <- dplyr::mutate(dplyr::rowwise(data),
                        var = split_extract(name, rm_last(seq_len(count_underscores(name)))),
                        val = split_extract(name, count_underscores(name))
  )
  data <- tidyr::spread(data, val, value)
  data <- dplyr::select(data, -name)
  data <- dplyr::group_by(data, var, !!!strat_symbols)
  data <- dplyr::summarise_all(data, list(~sum(., na.rm = TRUE)))
  dplyr::ungroup(data)
}

missing_data_make_plot <- function(plot_data) {
  max_plot_val <- max(plot_data[["value"]])
  p <- plot_data %>%
    ggplot(aes(x = fct_rev(key), y = value)) +
    ggalt::geom_lollipop() +
    geom_text(
      aes(label = str_c(value, "% (", value_absolute, ")")),
      nudge_y = max_plot_val * 0.1,
      fontface = "bold"
    ) +
    coord_flip() +
    labs(
      x = "Variable",
      y = "Proportion missing (%)",
      caption = MISSING_DATA_AGE_FOOTNOTE
    ) +
    hrbrthemes::theme_ipsum(
      axis_text_size = 14, axis_title_size = 16,
      base_family = "Palatino", grid = ""
    )
  if (max_plot_val <= 0.0001) {
    p <- p + scale_y_continuous(limits = c(0, 0.1))
  }
  p
}

MISSING_DATA_AGE_FOOTNOTE <- "* The percentage of missing values are based on dates that have either or both month and year of birth missing."
