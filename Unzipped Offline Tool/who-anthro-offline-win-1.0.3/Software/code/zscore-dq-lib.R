zscore_summary_cols <- c("zlen", "zwei", "zbmi", "zwfl")
zscore_flag_cols <- paste0(zscore_summary_cols, "_flag")

zscore_plot_flagged_zscores <- function(df_zscores) {
  df_zscores <- set_flagged_zscores_to_na(df_zscores)
  tmp <-
    df_zscores %>%
    select(one_of(zscore_flag_cols)) %>%
    summarise_all(funs(sum(. == 1, na.rm = TRUE) / n() * 100)) %>%
    plyr::ldply() %>%
    setNames(c("key", "value")) %>%
    tbl_df()
  tmp %>%
    ggplot(aes(x = fct_rev(key), y = value)) +
    ggalt::geom_lollipop() +
    geom_text(aes(label = str_c(round(value, 1), "%")),
              nudge_y = max(tmp$value) * 0.1, fontface = "bold"
    ) +
    coord_flip() +
    labs(x = "", y = "Flagged (%)") +
    hrbrthemes::theme_ipsum(
      base_size = 14, axis_title_size = 16,
      base_family = "Palatino", grid = ""
    )
}

zscore_prepare_plot_dist_data <- function(df_zscores,
                                          stratification_var_name,
                                          list_map_vars) {
  flag_available <-
    df_zscores %>%
    summarise_at(vars(matches("_flag$")), funs(all(is.na(.)))) %>%
    select_if(function(x) x == FALSE) %>%
    names()

  other_grouping_name <- anthro_make_grouping_label(list_map_vars[["othergr"]])
  if (stratification_var_name == "Other grouping variable") {
    stratification_var_name <- other_grouping_name
  }
  df_strat <-
    tibble(
      key = c(
        "Age group (months)", "Sex", "Residence type", "Geographical region",
        "Wealth quintile", "Mother education", "Team", other_grouping_name
      ),
      value = c(
        "age_group", list_map_vars[["sex"]], list_map_vars[["typeres"]],
        list_map_vars[["gregion"]], list_map_vars[["wealthq"]],
        list_map_vars[["mothered"]], list_map_vars[["team"]],
        list_map_vars[["othergr"]]
      )
    ) %>%
    filter(!value %in% "None") %>%
    # with the 'other grouping' option, the user could select
    # an already selected field
    distinct(value, .keep_all = TRUE)

  # in the case above, we need to map the stratification variable to
  # the already present one
  strat_row <- df_strat[df_strat$value == stratification_var_name, , drop = FALSE]
  if (nrow(strat_row) == 1L) {
    stratification_var_name <- strat_row$key
  }

  df_zscore_dist <-
    df_zscores %>%
    select(one_of(df_strat$value), one_of(flag_available) - 1) %>%
    mutate_at(df_strat$value, funs(as.factor(.))) %>%
    gather(key, value, -tidyselect::one_of(df_strat$value)) %>%
    filter(!is.na(value)) %>%
    mutate(key = fct_relevel(key, "zlen", "zwfl", "zwei", "zbmi")) %>%
    mutate(key = fct_recode(
      key,
      `Length- or height-for-age` = "zlen",
      `Weight-for-length or height` = "zwfl",
      `Weight-for-age` = "zwei",
      `Body mass index-for-age` = "zbmi"
    ))
  indx <- df_zscore_dist %>% names() %>% match(df_strat$value, nomatch = 0)
  df_zscore_dist %<>% rename_at(names(.)[indx != 0], ~df_strat$key[indx])

  if (!is.null(df_zscore_dist$Sex)) {
    df_zscore_dist <-
      df_zscore_dist %>%
      mutate(
        Sex = recode_sex_factor(Sex)
      )
  }

  if (!is.null(df_zscore_dist[["Wealth quintile"]])) {
    df_zscore_dist <-
      df_zscore_dist %>%
      mutate(
        `Wealth quintile` = recode_wiq_factor(`Wealth quintile`)
      )
  }
  if (is.null(df_zscore_dist[[stratification_var_name]])) {
    n_levels <- NA_integer_
  } else {
    n_levels <- dplyr::n_distinct(df_zscore_dist[[stratification_var_name]])
  }
  list(
    n_levels = n_levels,
    df_zscore_dist = df_zscore_dist,
    stratification_var_name = stratification_var_name
  )
}

zscore_plot_distribution <- function(data) {
  stratification_var_name <- data[["stratification_var_name"]]
  df_zscore_dist <- data[["df_zscore_dist"]]
  n_levels <- data[["n_levels"]]
  if (!is.null(stratification_var_name) &&
        is_label_wiq(stratification_var_name) &&
        is.factor(df_zscore_dist[[stratification_var_name]])) {
    df_zscore_dist[[stratification_var_name]] <- recode_wiq_factor(df_zscore_dist[[stratification_var_name]])
  }

  p <- if (stratification_var_name == "None") {
    df_zscore_dist %>%
      ggplot() +
      stat_density(aes(value, linetype = "Samples"),
                   size = .25,
                   geom = "line", position = "identity"
      ) +
      anthro_ggplot2_standard_normal(linetype_as_aes = TRUE) +
      facet_wrap(~key) +
      xlim(-6, 6) +
      labs(x = "z-scores", y = "Density") +
      hrbrthemes::theme_ipsum(base_family = "Palatino", grid = "") +
      anthro_scale_linetype_manual_no_groups
  } else if (!is.na(n_levels) && n_levels > 2) {
    facet_formula <- rlang::new_formula(
      as.symbol(stratification_var_name),
      quote(key)
    )
    df_zscore_dist %>%
      # sometimes the stratification variable can have NAs,
      # we generally filter out these cases.
      # It can happen if a row has an age that larger than 60 months,
      # then the age group is NA.
      filter(!is.na(!!as.symbol(stratification_var_name))) %>%
      zscore_plot_dist_facet(facet_formula) +
      anthro_scale_linetype_manual_no_groups
  } else {
    df_zscore_dist %>%
      ggplot() +
      stat_density(
        aes_string("value", linetype = stratification_var_name %>% as.name()),
        size = .25,
        geom = "line", position = "identity"
      ) +
      anthro_ggplot2_standard_normal() +
      anthro_ggplot2_standard_normal_caption() +
      facet_wrap(~key, scales = "free") +
      xlim(-6, 6) +
      labs(x = "z-scores", y = "Density") +
      hrbrthemes::theme_ipsum(
        base_size = 14, axis_title_size = 16,
        base_family = "Palatino", grid = ""
      ) +
      theme(legend.key = element_blank())
  }
  p + theme(legend.position = "bottom")
}

zscore_plot_dist_facet <- function(data, facet_formula) {
  ggplot(data) +
    stat_density(
      aes(value, linetype = "Samples"),
      size = .25,
      geom = "line", position = "identity"
    ) +
    anthro_ggplot2_standard_normal(linetype_as_aes = TRUE) +
    facet_grid(facet_formula) +
    xlim(-6, 6) +
    labs(x = "z-scores", y = "Density") +
    hrbrthemes::theme_ipsum(base_family = "Palatino", grid = "") +
    theme(
      panel.spacing.x = unit(.04, "cm"),
      panel.spacing.y = unit(.02, "cm")
    )
}

set_flagged_zscores_to_na <- function(zscore_dataframe) {
  zscore_flagged_cols <- paste0(zscore_summary_cols, "_flag")
  stopifnot(c(zscore_summary_cols, zscore_flagged_cols) %in% colnames(zscore_dataframe))

  for (i in seq_len(length(zscore_summary_cols))) {
    col <- zscore_summary_cols[[i]]
    col_flag <- zscore_flagged_cols[[i]]
    is_flagged <- !is.na(zscore_dataframe[[col_flag]]) &
      zscore_dataframe[[col_flag]] == 1L
    zscore_dataframe[[col]][is_flagged] <- NA_real_
  }

  zscore_dataframe
}


zscore_create_col_labels <- function(col) {
  paste0(c(
    "Mean",
    "Standard deviation",
    "Skewness",
    "Kurtosis"
  ), " (", col, ")")
}

zscore_summary_groups <- function(list_map_vars) {
  relevant_variables <- c("team", "typeres", "gregion", "othergr")
  prefixes <- list(
    team = "Team", typeres = "Area",
    gregion = "Geographical region",
    othergr = "Other grouping"
  )
  groups <- lapply(relevant_variables, function(var) {
    list(
      prefix = prefixes[[var]],
      column = list_map_vars[[var]]
    )
  })
  Filter(function(x) is_variable_mapped(x$column), groups)
}

zscore_compute_summary_data <- function(zscores_wo_flagged, zscore_summary_groups) {
  res <- summarise_zscore_data(
    zscores_wo_flagged,
    groups = zscore_summary_groups
  )
  zscore_col_names <- unlist(lapply(zscore_summary_cols, zscore_create_col_labels))
  col_names <- c("Group", "Unweighted N", zscore_col_names)
  colnames(res) <- col_names
  res
}

# zscore summaries are always done for "all", age groups and sex
# groups are list of lists
# each sublist has the following information: prefix, column
summarise_zscore_data <- function(data, groups = list()) {
  if (!is.list(groups)) groups <- list()
  relevant_columns <- c(
    "age_group", "csex",
    vapply(groups, function(x) x$column, character(1L)),
    zscore_summary_cols
  )
  stopifnot(relevant_columns %in% colnames(data))

  summary_data <- data[, relevant_columns, drop = FALSE]

  strat_groups <- c(
    list(list(prefix = "All")),
    list(list(prefix = "Age group", column = "age_group")),
    list(list(prefix = "Sex", column = "csex")),
    Filter(function(g) g$prefix == "Team", groups),
    Filter(function(g) g$prefix != "Team", groups)
  )

  compute_statistics <- function(group, zscore_data) {
    result <- tibble::tibble(group,
                             n = nrow(zscore_data)
    )
    rd <- function(x) scales::number(round(x, 2L), accuracy = 0.01)
    for (col in zscore_summary_cols) {
      zscores <- zscore_data[[col]]
      stats <- tibble::tibble(
        mean = rd(mean(zscores, na.rm = TRUE)),
        sd = rd(sd(zscores, na.rm = TRUE)),
        skewness = rd(moments::skewness(zscores, na.rm = TRUE)),
        kurtosis = rd(moments::kurtosis(zscores, na.rm = TRUE))
      )
      colnames(stats) <- paste0(col, "_", colnames(stats))
      result <- cbind(result, stats)
    }
    result
  }


  statistics_by_group <- lapply(strat_groups, function(group) {
    if (is.null(group$column)) {
      compute_statistics("All", summary_data)
    } else {
      res <- dplyr::group_by(summary_data, !!as.symbol(group$column))
      res <- dplyr::ungroup(dplyr::do(res, {
        compute_statistics("tmp", .)
      }))

      if (group$prefix == "Sex") {
        res[[group$column]] <- stringr::str_replace_all(
          res[[group$column]],
          c(
            "^1$" = "Male",
            "^2$" = "Female",
            "^M$" = "Male",
            "^F$" = "Female"
          )
        )
      }
      res <- dplyr::filter(res, !is.na(!!as.symbol(group$column)))
      res[["group"]] <- paste0(group$prefix, ": ", res[[group$column]])
      res[[group$column]] <- NULL
      res
    }
  })

  dplyr::bind_rows(statistics_by_group)
}
