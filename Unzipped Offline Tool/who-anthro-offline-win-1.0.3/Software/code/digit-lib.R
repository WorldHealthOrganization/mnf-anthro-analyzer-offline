digit_prepare_heaping_plot_data <- function(df_zscores, weight_col, lenhei_col, strat_column) {
  digit_preference <-
    df_zscores %>%
    select(
      !!as.symbol(weight_col),
      !!as.symbol(lenhei_col)
    ) %>%
    mutate_all(round, digits = 1) %>%
    mutate_all(funs(as.character)) %>%
    mutate_all(funs(ifelse(str_detect(., "\\.") %in% FALSE, paste0(., ".0"), .))) %>%
    mutate_all(str_extract, pattern = "\\..*") %>%
    mutate_all(funs(ifelse(is.na(.), NA_character_, paste0("0", .))))
  if (!is.null(strat_column)) {
    digit_preference %<>% bind_cols(df_zscores[, strat_column, drop = FALSE]) %>%
      gather(key, value, -strat_column) %>%
      filter(!is.na(!!as.symbol(strat_column))) %>%
      mutate(key = fct_inorder(key)) %>%
      group_by(key, value, !!as.symbol(strat_column)) %>%
      tally() %>%
      group_by(key, !!as.symbol(strat_column))
  } else {
    digit_preference %<>% gather(key, value) %>%
      mutate(key = fct_inorder(key)) %>%
      group_by(key, value) %>%
      tally() %>%
      group_by(key)
  }

  digit_preference %<>%
    filter(!is.na(value)) %>%
    mutate(prop = (n / sum(n) * 100) %>% round(1)) %>%
    ungroup()
  if (!is.null(strat_column)) {
    digit_preference[["strat_var"]] <- digit_preference[[strat_column]]
  }
  digit_preference$pretty_key <-
    factor(digit_preference$key, labels = c("Weight (kg)", "Length or height (cm)"))
  digit_preference
}

digit_create_heaping_plot <- function(plot_data,
                                      is_stratification_variable_selected) {
  p <- plot_data %>%
    ggplot(aes(x = value %>% fct_rev(), y = prop)) +
    ggalt::geom_lollipop() +
    coord_flip() +
    labs(x = "Digit", y = "Proportion (%)") +
    anthro_ggplot2_style()
  if (is_stratification_variable_selected) {
    p + facet_grid(strat_var ~ pretty_key)
  } else {
    p + facet_wrap(~pretty_key, ncol = 2) +
      geom_text(
        aes(label = str_c(prop, "%")),
        nudge_y = max(plot_data$prop) * 0.05,
        fontface = "bold"
      )
  }
}

digit_weight_bounds <- c(2L, 25L)
digit_height_bounds <- c(35L, 125L)

digit_count_integers <- function(dataset, value_column, bounds) {
  req(value_column %in% colnames(dataset))
  values <- dataset[[value_column]]
  values <- as.integer(values)
  values <- values[values >= bounds[1L] & values <= bounds[2L]]
  res <- dplyr::group_by(data.frame(value = values), .data$value)
  dplyr::count(res)
}

digit_prepare_weight_plot_data <- function(dataset, weight_col) {
  digit_count_integers(dataset, weight_col, digit_weight_bounds)
}

digit_weight_plot <- function(plot_data) {
  digit_plot_integer_histogram(plot_data, "Weight (kg) -", limits = digit_weight_bounds)
}

digit_prepare_height_plot_data <- function(dataset, height_col) {
  digit_count_integers(dataset, height_col, digit_height_bounds)
}

digit_height_plot <- function(plot_data) {
  digit_plot_integer_histogram(plot_data, "Length or height (cm) -",
                         limits = digit_height_bounds, step_size = 5
  )
}

digit_plot_integer_histogram <- function(plot_data, xlab, limits, step_size = 1) {
  stopifnot(is.data.frame(plot_data), c("value", "n") %in% colnames(plot_data))
  stopifnot(length(xlab) == 1L, is.character(xlab))
  stopifnot(length(limits) == 2L, is.numeric(limits))
  breaks <- seq(limits[1L], limits[2L], step_size)
  plot_data <- dplyr::filter(plot_data, value >= limits[1L], value <= limits[2L])
  ggplot(plot_data) +
    aes(x = value, y = n) +
    geom_bar(stat = "identity") +
    anthro_ggplot2_style() +
    scale_x_continuous(limits = limits + c(-1, 1), breaks = breaks) +
    xlab(paste0(xlab, " Digit (integer)")) +
    ylab("Frequency")
}
