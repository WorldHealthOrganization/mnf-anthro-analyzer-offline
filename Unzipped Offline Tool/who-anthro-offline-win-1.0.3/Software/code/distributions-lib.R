# Functions used for the distribution module
# Mostly extracted from the module

dist_prepare_plot_data <- function(dataset) {
  # we need to add a column that has the age in years in it
  # this column is only visible to this module
  dataset$plot_age_in_years <- cut(dataset$age_in_months, breaks = c(0, 12, 24, 36, 48, 60), right = FALSE, labels = 0:4)
  dataset$plot_age_in_months <- cut(dataset$age_in_months, 0:60, right = FALSE, labels = 0:59)
  dataset
}

age_in_months_to_years <- function(age_in_months) {
  stopifnot(is.numeric(age_in_months))
  if (length(age_in_months) == 0) {
    return(age_in_months)
  }
  age_in_months_ret <- rep.int(NA_integer_, length(age_in_months))
  age_in_months_ret[age_in_months < 12] <- 0L
  age_in_months_ret[age_in_months >= 12 & age_in_months < 24] <- 1L
  age_in_months_ret[age_in_months >= 24 & age_in_months < 36] <- 2L
  age_in_months_ret[age_in_months >= 36 & age_in_months < 48] <- 3L
  age_in_months_ret[age_in_months >= 48 & age_in_months < 60] <- 4L
  age_in_months_ret
}

dist_plot <- function(plot_data,
                      x_variable,
                      x_variable_label,
                      is_x_discrete,
                      stratification_variable,
                      stratification_variable_label) {
  is_x_sex <- x_variable_label == "Sex"
  is_x_wiq <- is_label_wiq(x_variable_label)
  if (is_x_discrete) {
    if (!is.factor(plot_data[[x_variable]])) {
      plot_data[[x_variable]] <- factor(plot_data[[x_variable]])
    }
    if (is_x_sex) {
      plot_data[[x_variable]] <- recode_sex_factor(plot_data[[x_variable]])
    } else if (is_x_wiq) {
      plot_data[[x_variable]] <- recode_wiq_factor(plot_data[[x_variable]])
    }
  }
  if (length(stratification_variable) == 0L) {
    plot_tmp <-
      if (is_x_discrete) {
        # bar plot
        ggplot(plot_data) +
          geom_bar(
            aes(x = !!as.symbol(x_variable))
          ) +
          scale_x_discrete(drop = FALSE) +
          theme_bw() +
          hrbrthemes::theme_ipsum(
            axis_text_size = 14, axis_title_size = 16,
            base_family = "Palatino", grid = ""
          ) +
          geom_label(
            stat = "count",
            aes(x = !!as.symbol(x_variable), label = ..count.., y = ..count..)
          ) +
          labs(x = x_variable_label)
      } else {
        # density plot
        ggplot(plot_data) +
          geom_density(
            aes_string(x = x_variable, y = "..density.."),
            fill = "gray75"
          ) +
          theme_bw() +
          hrbrthemes::theme_ipsum(
            axis_text_size = 14, axis_title_size = 16,
            base_family = "Palatino", grid = ""
          ) +
          theme(legend.position = "none") +
          labs(x = x_variable_label)
      }
  } else {
    req(length(stratification_variable) == 1L && stratification_variable %in% colnames(plot_data))

    is_strat_sex <- stratification_variable_label == "Sex"
    is_strat_wiq <- is_label_wiq(stratification_variable_label)

    plot_data[[stratification_variable]] <- factor(as.character(plot_data[[stratification_variable]]),
      levels = sort(unique(plot_data[[stratification_variable]]))
    )
    plot_data[[stratification_variable]] <- forcats::as_factor(plot_data[[stratification_variable]])
    if (is_strat_sex) {
      plot_data[[stratification_variable]] <- recode_sex_factor(plot_data[[stratification_variable]])
    } else if (is_strat_wiq) {
      plot_data[[stratification_variable]] <- recode_wiq_factor(plot_data[[stratification_variable]])
    }
    plot_tmp <-

      if (is_x_discrete) {
        ggplot(plot_data) +
          aes(x = !!as.symbol(x_variable), fill = !!as.symbol(stratification_variable)) +
          geom_bar() +
          scale_x_discrete(drop = FALSE) +
          theme_bw() +
          hrbrthemes::theme_ipsum(
            axis_text_size = 14, axis_title_size = 16,
            base_family = "Palatino", grid = ""
          ) +
          geom_label(
            stat = "count",
            aes(x = !!as.symbol(x_variable), label = ..count.., y = ..count..),
            position = position_stack(vjust = 0.5)
          ) +
          scale_fill_manual(
            name = stratification_variable_label,
            values = gray.colors(dplyr::n_distinct(plot_data[[stratification_variable]]), end = 0.7)
          ) +
          labs(x = x_variable_label)
      } else {
        # violin plot
        ggplot(plot_data) +
          geom_violin(
            aes_string(
              x = stratification_variable,
              y = x_variable, fill = stratification_variable
            ),
            na.rm = TRUE
          ) +
          scale_fill_manual(
            name = stratification_variable_label,
            values = gray.colors(dplyr::n_distinct(plot_data[[stratification_variable]]), end = 0.7)
          ) +
          coord_flip() +
          theme_bw() +
          hrbrthemes::theme_ipsum(
            axis_text_size = 14, axis_title_size = 16,
            base_family = "Palatino", grid = ""
          ) +
          theme(legend.position = "none") +
          labs(
            y = x_variable_label,
            x = stratification_variable_label
          )
      }
  }
  plot_tmp
}

dist_compute_mismatch_table <- function(data, measure_col, age_in_month_col) {
  age_groups <- list(
    c(0L, 12L),
    c(0L, 9L),
    c(12L, 24L),
    c(24L, 36L),
    c(36L, 48L),
    c(48L, 60L),
    as.integer(c(0, max(data[[as.character(age_in_month_col)]], na.rm = TRUE) + 100))
  )
  data <- dplyr::filter(data, !is.na(!!measure_col))
  res <- lapply(age_groups, function(limit) {
    local_data <- dplyr::filter(
      data, !!age_in_month_col >= limit[1],
      !!age_in_month_col < limit[2]
    )
    age_label <- sprintf("%02d-%02d mo", limit[1], limit[2] - 1)
    if (limit[1] == 0L && limit[2] == 9L) {
      age_label <- paste0("&nbsp;&nbsp;", age_label)
    }
    dplyr::summarise(local_data,
      age_group = age_label,
      total = dplyr::n(),
      mismatch = sum(
        (is_measure_height(!!measure_col) & .data$age_in_months < 24) |
          is_measure_length(!!measure_col) & .data$age_in_months >= 24,
        na.rm = TRUE
      ),
      rel = .data$mismatch / .data$total
    )
  })
  res <- dplyr::bind_rows(res)
  res[["age_group"]][length(res[["age_group"]])] <- "Total"
  res[["Expected Position"]] <- c(
    "lying", "lying", "lying",
    "standing", "standing", "standing", ""
  )
  res <- res[, c(1, 5, 2, 3, 4)]
  res <- dplyr::mutate(dplyr::filter(res, is.finite(rel)),
    rel = scales::percent(rel)
  )
  colnames(res) <- c(
    "Age group", "Expected position",
    "Total", "Observed mismatch*", "% mismatch*"
  )
  res
}

dist_mismatch_n_children <- function(data, lenhei_unit_col) {
  sum(is.na(data[[lenhei_unit_col]]))
}
