## Function for calculating the prevalences for all indicators
CalculatePrev <- function(data, sex = NULL, age = NULL, age.month = FALSE, date_obs = NULL,
                          date_birth = NULL, weight = NULL, lenhei = NULL, lenhei_unit = NULL,
                          headc = NULL, armc = NULL, triskin = NULL, subskin = NULL,
                          typeres = NULL, wealthq = NULL, gregion = NULL, mothered = NULL,
                          othergr = NULL, sw = NULL, cluster = NULL, strata = NULL, oedema = NULL,
                          on_progress = function() invisible(TRUE)) {
  # check input
  # the code now takes the age group and age_in_days from the input data and does not
  # compute it again
  stopifnot(all(c("age_in_days", "age_group") %in% colnames(data)))

  # on_progress is a callback function that is called when big
  # chunks of the computation finish in order to signal progress to the user
  stopifnot(is.function(on_progress))

  # Remove empty rows
  data %<>% filter(!rowSums(is.na(.)) %in% ncol(.))

  ## if strata is missing, assign it a value of 1 - else won't work!
  strata_not_defined <- is.null(strata) || strata == "None"
  if (strata_not_defined) {
    data$strata <- 1
    strata <- "strata"
  }
  cluster_not_defined <- is.null(cluster) || cluster == "None"
  if (cluster_not_defined) {
    data$cluster <- 1
    cluster <- "cluster"
  }
  sw_not_defined <- is.null(sw) || sw == "None"
  if (sw_not_defined) {
    data$sw <- 1
    sw <- "sw"
  }

  ## create dataframe of missing/unmatched arguments
  list_missing_args <-
    list(sex, age, date_obs, date_birth, lenhei_unit, oedema, typeres, wealthq, gregion, mothered, othergr, sw, weight, lenhei, cluster, strata, "extra_var") %>%
    set_names(c(
      "sex", "age", "date_obs", "date_birth", "lenhei_unit", "oedema", "typeres", "wealthq", "gregion", "mothered", "othergr", "sw", "weight",
      "lenhei", "cluster", "strata", "extra_var"
    )) %>%
    plyr::ldply(function(x) if (is.null(x) || x %in% c("None", "extra_var")) NA)

  available_vars <-
    c(sex, age, lenhei_unit, oedema, typeres, wealthq, gregion, mothered, othergr, sw, weight, lenhei, cluster, strata)

  ## create new variables based on these missing arguments; map the arguments to these variables
  for (i in list_missing_args[[1]]) assign(i, i)

  if (any(sw < 0)) stop("N.B. Negative weights are not allowed and programme will stop. Prevalence tables will not be produced.")

  ## Recasting sex variable
  data[[sex]] <- stringr::str_replace_all(data[[sex]],
                                          c("^M$" = "Male", "^F$" = "Female",
                                            "^1$" = "Male", "^2$" = "Female"))

  # retain only those records where age <= 1826 or is.na(age)
  data <- data[is.na(data[["age_in_days"]]) | data[["age_in_days"]] <= 1826, ]

  ## Create sex x age group variable
  data$age_sex <- interaction(data$age_group, data[[sex]])

  ## Make z-score as missing if it is flagged
  flag_posn <- grep("_flag$", names(data))
  table_z <- tibble::tibble(zscore = names(data)[flag_posn - 1], flag = names(data)[flag_posn])

  ChangeZScore <- function(x) {
    z_name <- table_z$zscore[x]

    data %>%
      select(grep(paste0("^", z_name), names(data))) %>%
      mutate(z_new = ifelse(.[[2]] == 1, NA, .[[1]])) %>%
      select(-1, -2) %>%
      setNames(z_name)
  }

  data[flag_posn - 1] <- lapply(1:nrow(table_z), ChangeZScore) %>% bind_cols()

  ## Make Oedema variable to be "n" if age > than 60 completed months
  ## (children w/ oedema count in prevalence even if z-score missing for weight related indicators)
  if (is_variable_mapped(oedema)) {
    data[[oedema]] <- ifelse((!is.na(data[["age_in_days"]]) & data[["age_in_days"]] > 1826) | is.na(data[[oedema]]), "n", as.character(data[[oedema]]))
  }


  #######################################################################################################
  #### Make weight-related indicators z-scores as missing if it is oedema="y" and create an auxiliar
  #### z-score variable with z-score=-3.1 if oedema="y" to be use only to compute prevalences of
  #### weight-related indicators - this way, it will count the child in the denominators always and will
  #### will also count the child in the numerator as well for undernutrition cut-offs.
  #### Note, for computing summary statistics for z-scores, the original z-score variables will be used,
  #### as information on measured weight is unavailable (or misleading if it is).
  #######################################################################################################
  stopifnot(all(c("zwei", "zwfl", "zbmi") %in% colnames(data)))
  for (i in c("zwei", "zwfl", "zbmi")) {
    oedema_yes <- if (is_variable_mapped(oedema)) {
      is_oedema_yes(data[[oedema]])
    } else {
      rep.int(FALSE, nrow(data))
    }
    data[[i]] <- ifelse(oedema_yes, NA_real_, data[[i]])
    data[[paste0(i, "_aux")]] <- ifelse(oedema_yes, -3.1, data[[i]])
  }

  ################################################################################################################
  ##### For the R Survey package, the id argument (cluster, PSU) is always required, the strata, fpc, weights
  ##### and probs arguments are optional. If these variables are specified they must not have any missing values.
  ##### We use cluster and strata in the function to define the design. If one of them is not given, they
  ##### are replaced by vectors of all 1's (see variable setting in the begining of macro). However, if
  ##### these variables are provided, we have to use the subset of records where those are not missing.
  ################################################################################################################
  if (!strata_not_defined) stopifnot(strata %in% colnames(data))
  if (!cluster_not_defined) stopifnot(cluster %in% colnames(data))
  if (!sw_not_defined) stopifnot(sw %in% colnames(data))
  df_aux <- dplyr::filter(
    data,
    if (strata_not_defined) TRUE else !is.na(!!(as.symbol(strata))),
    if (cluster_not_defined) TRUE else !is.na(!!(as.symbol(cluster))),
    if (sw_not_defined) TRUE else !is.na(!!(as.symbol(sw)))
  )

  table_cutoff <-
    tibble::tibble(
      cutoffs = c("< -3", "< -2", "< -1", "> 1", "> 2", "> 3"),
      cutoffs_lab = c("_3", "_2", "_1", "1", "2", "3")
    )

  table_z <-
    tibble::tibble(
      z_prev = c("zlen", "zwei_aux", "zwfl_aux", "zbmi_aux"),
      z_summ = c("zlen", "zwei", "zwfl", "zbmi"),
      z_lab = c("HA", "WA", "WH", "BMI")
    )

  ## Total sample and by age groups
  remove_cols <-
    1:3 %>%
    map(~seq(., 188, 47)) %>%
    unlist() %>%
    sort() %>%
    map(~seq(., . + 7 * 5, by = 7)) %>%
    unlist() %>%
    sort() %>%
    magrittr::extract(-c(1, 2, 3, 20, 21, 38, 39, 56, 57))

  other_grouping_name <- anthro_make_grouping_label(othergr)

  df_poststr <-
    tibble::tibble(
      key = c("age_group", sex, "age_sex", typeres, gregion, wealthq, mothered, othergr),
      value = c(TRUE, rep(FALSE, 7)), #the first value needs to be true
      pretty_key = c("Age group", "Sex", "Age + sex", "Area", "Geographical region", "Wealth quintile", "Maternal education", other_grouping_name)
    ) %>%
    filter(key %in% c("age_group", "age_sex", available_vars))
  on_progress()

  suppressWarnings(
    list_estimates_tot <-
      lapply(seq_len(nrow(df_poststr)), function(i) {
        on_progress()
        CalculateAllPrevCols(
          data = df_aux, poststr_var = as.character(df_poststr[i, 1]), remove = remove_cols,
          total = as.character(df_poststr[i, 2]), cluster_var = cluster, strata_var = strata,
          sw_var = sw, table_z_var = table_z, table_cutoff_var = table_cutoff,
          pretty_name = as.character(df_poststr[i, 3])
        )
      }) %>%
      plyr::ldply(.) %>%
      tbl_df()
  )

  # if oedema is mapped we count the oedema cases per stratum
  # and add it as a column to the resulting dataset
  if (is_variable_mapped(oedema)) {
    # this is most likely always true even if oedema is not mapped
    # but I leave it here to make the code a bit more robust in case
    # something changes
    oedema_counts <- count_oedema_yes(df_aux, df_poststr[["key"]], oedema)

    # sometimes certain levels do not have any results. The function
    # above only computes the results for the levels that do have at least
    # one row
    non_na_values <- !is.na(list_estimates_tot[["HAZ_unwpop"]])
    if (sum(non_na_values) == length(oedema_counts)) {
      list_estimates_tot[["Oedema_cases"]][non_na_values] <- oedema_counts
    }
  }
  list_estimates_tot <-
    list_estimates_tot %>%
    mutate(
      rowname =
        str_replace_all(
          rowname,
          c(
            "^Wealth quintile: Q?1$" = "Wealth quintile: Q1: Poorest",
            "^Wealth quintile: Q?2$" = "Wealth quintile: Q2",
            "^Wealth quintile: Q?3$" = "Wealth quintile: Q3",
            "^Wealth quintile: Q?4$" = "Wealth quintile: Q4",
            "^Wealth quintile: Q?5$" = "Wealth quintile: Q5: Richest"
          )
        )
    )
  dplyr::rename(list_estimates_tot, Group = rowname)
}

# this function takes a dataset and counts the oedema = yes cases
# by group
# this is needed to report the number of oedema cases per stratum
count_oedema_yes <- function(data, stratifications, oedema_col) {
  total_oedema <- sum(is_oedema_yes(data[[oedema_col]]))
  res <- lapply(stratifications, function(group) {
    df <- dplyr::filter(data, !is.na(!!as.symbol(group)))
    df <- dplyr::group_by(df, !!as.symbol(group))
    df <- dplyr::summarise(df, oedema_yes = sum(is_oedema_yes(!!(as.symbol(oedema_col)))))
    df[["oedema_yes"]]
  })
  c(total_oedema, unlist(res))
}
