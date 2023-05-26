## Function for calculating the z-scores for all indicators
CalculateZScores <- function(data, sex = NULL,
                             weight = NULL, lenhei = NULL, lenhei_unit = NULL,
                             oedema = NULL) {
  stopifnot(c("age_in_days", "age_group", "age_in_months") %in% colnames(data))

  # remove column for data that will be added later to prevent bugs
  col_names <- colnames(data)
  zscore_cols <- c("zwfl", "zbmi", "zlen", "zwei")
  col_names <- setdiff(col_names, c(zscore_cols, paste0(zscore_cols, "_flag")))
  data <- data[, col_names, drop = FALSE]

  # Remove empty rows
  data %<>% filter(!rowSums(is.na(.)) %in% ncol(.))

  ## create dataframe of missing/unmatched arguments
  list_missing_args <-
    list(sex, lenhei_unit, oedema, weight, lenhei, "extra_var") %>%
    set_names(c("sex", "lenhei_unit", "oedema", "weight", "lenhei", "extra_var")) %>%
    plyr::ldply(function(x) if (is.null(x) || x %in% c("None", "extra_var")) NA)

  # For children that are younger than 9 months and lenhei_unit is H we set lenhei_unit to missing
  # The reason is that the measurement is likely implausible
  # this adds another column to the resulting dataset
  lenhei_unit_mapped <- is_variable_mapped(lenhei_unit)
  if (lenhei_unit_mapped) {
    data[["cmeasure"]] <- adjust_measure(data[[lenhei_unit]], data[["age_in_months"]])
  } else {
    data[["cmeasure"]] <- NA_character_
  }

  ## create new variables based on these missing arguments; map the arguments to these variables
  for (i in list_missing_args[[1]]) {
    data[[i]] <- NA
    assign(i, i)
  }

  # this code is a fix due to the odd way this function was initially written
  data[["lenhei_unit"]] <- NULL
  lenhei_unit <- "cmeasure"

  ## add unique id variable
  data %<>% mutate(unique_id = row_number())

  if (!is.null(weight) || weight != "None") data[[weight]] %<>% as.numeric
  if (!is.null(lenhei) || lenhei != "None") data[[lenhei]] %<>% as.numeric

  data[[weight]] <- ifelse(between(data[[weight]], 0.9, 58.0), data[[weight]], NA)
  data[[lenhei]] <- ifelse(between(data[[lenhei]], 38.0, 150.0), data[[lenhei]], NA)


  ## truncate age in days
  data[["age_in_days"]] <- ifelse(data[["age_in_days"]] < 0, NA_real_, data[["age_in_days"]])

  # the zscore function alter the age in days, we do not want that
  old_age_days <- data[["age_in_days"]] # negative ages will be cast to NA

  ## if child is =< 730 days, lenhei_unit var should be 'L'. If lenhei_unit var is 'H', must add 0.7cm to standardise
  ## if child is > 730 days, lenhei_unit var should be 'H'. If lenhei_unit var is 'L', must subtract 0.7cm to standardise
  rounded_age_in_days <- round_up(data[["age_in_days"]])
  data$clenhei <-
    ifelse(!is.na(rounded_age_in_days) & rounded_age_in_days < 731 & !is.na(data[[lenhei_unit]]) &
      (data[[lenhei_unit]] == "h" | data[[lenhei_unit]] == "H"),
    data[[lenhei]] + 0.7,
    ifelse(!is.na(rounded_age_in_days) & rounded_age_in_days >= 731 & !is.na(data[[lenhei_unit]]) &
      (data[[lenhei_unit]] == "l" | data[[lenhei_unit]] == "L"),
    data[[lenhei]] - 0.7,
    data[[lenhei]]
    )
    )

  ## calculate BMI
  data$cbmi <- data[[weight]] / (data$clenhei / 100)^2

  ## Data cleaning
  ## Standardise variable encoding
  data$csex <-
    ifelse(data[[sex]] %in% c("m", "M", "1", 1), 1, ifelse(data[[sex]] %in% c("f", "F", "2", 2), 2, NA)) %>%
    as.integer()

  data[[lenhei_unit]] <-
    if (data[[lenhei_unit]] %>% is.na() %>% all()) {
      data[[lenhei_unit]]
    } else {
      ifelse(data[[lenhei_unit]] %in% c("l", "L", "h", "H"), data[[lenhei_unit]], NA)
    }

  ## what about NA for oedema?? Needs 'No' for current version of 'prevalence.R'
  oedema_yes <- is_oedema_yes(data[[oedema]])
  data[[oedema]] <- ifelse(oedema_yes, "y", "n")

  ## Calculate z-scores and add as new columns to dataset
  data %<>%

    # 1st set
    ## Length-for-age z-score
    MakeZScores1(
      growth_standard = list_standards[["lenanthro"]], measure = "clenhei", zscore_name = "zlen",
      flag_name = "zlen_flag", flag_max = 6, agevar = "age_in_days", sexvar = "csex",
      condition = "!is.na(data[[agevar]]) & data[[agevar]] >= 0 & data[[agevar]] <= 1856"
    ) %>%

    # 2nd set
    ## Weight-for-age z-score
    MakeZScores2(
      growth_standard = list_standards[["weianthro"]], measure = weight, zscore_name = "zwei",
      flag_name = "zwei_flag", flag_max = 5, flag_min = -6, agevar = "age_in_days", sexvar = "csex", oedemavar = oedema,
      condition = '!is.na(data[[agevar]]) & data[[agevar]] >= 0 & data[[agevar]] <= 1856 & !is_oedema_yes(data[[oedemavar]])'
    ) %>%
    ## BMI-for-age z-score
    MakeZScores2(
      growth_standard = list_standards[["bmianthro"]], measure = "cbmi", zscore_name = "zbmi",
      flag_name = "zbmi_flag", flag_max = 5, agevar = "age_in_days", sexvar = "csex", oedemavar = oedema,
      condition = '!is.na(data[[agevar]]) & data[[agevar]] >= 0 & data[[agevar]] <= 1856 & !is_oedema_yes(data[[oedemavar]])'
    ) %>%
    select(-contains("loh"))

  # 3rd set
  ## Weight-for-length z-score
  data[["age_in_days"]] <- old_age_days #handle side effects of functions
  wfl <-
    MakeZScores3(data,
      growth_standard = list_standards[["wflanthro"]], measure = weight, length_measure = "length", zscore_name = "zwfl", flag_name = "zwfl_flag", flag_max = 5, agevar = "age_in_days", sexvar = "csex",
      lenheivar = "clenhei", lenhei_unitvar = lenhei_unit, oedemavar = oedema,
      condition = '!is_oedema_yes(data[[oedemavar]]) & (((!is.na(data[[agevar]]) & data[[agevar]] < 731) | (is.na(data[[agevar]]) & ((data[[lenhei_unitvar]] == "l" | data[[lenhei_unitvar]] == "L") | (is.na(data[[lenhei_unitvar]]) & !is.na(data[[lenheivar]]) & (data[[lenheivar]] < 87))))) & (data[[lenheivar]] >= 45 & data[[lenheivar]] <= 110))'
    )

  ## Weight-for-height z-score
  data[["age_in_days"]] <- old_age_days #handle side effects of functions
  wfh <-
    MakeZScores3(data, growth_standard = list_standards[["wfhanthro"]], measure = weight, length_measure = "height", zscore_name = "zwfl", flag_name = "zwfl_flag", flag_max = 5, agevar = "age_in_days", sexvar = "csex", lenheivar = "clenhei", lenhei_unitvar = lenhei_unit, oedemavar = oedema, condition = '!data[[oedemavar]] %in% "y" & (((!is.na(data[[agevar]]) & data[[agevar]] >= 731) | (is.na(data[[agevar]]) & ((data[[lenhei_unitvar]] == "h" | data[[lenhei_unitvar]] == "H") | (is.na(data[[lenhei_unitvar]]) & !is.na(data[[lenheivar]]) & (data[[lenheivar]] >= 87))))) & (data[[lenheivar]] >= 65 & data[[lenheivar]] <= 120))')

  data[["age_in_days"]] <- old_age_days

  ## bind wfh & wfl, and merge this back with dataframe
  data <- data %>%
    left_join(
      bind_rows(wfh, wfl),
      by = "unique_id"
    ) %>%
    select(-unique_id, -extra_var) %>%
    select(-tidyselect::matches("headc|zhc|armc|zac|triskin|zts|subskin|zss")) %>%
    tbl_df()
  data
}

adjust_measure <- function(measure, age_in_months) {
  measure_implausible <- (measure == "h" | measure == "H") & age_in_months < 9
  measure[measure_implausible] <- NA_character_
  measure
}
