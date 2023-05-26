# helper function
round_age_var_for_join <- function(x) {
  # for joining we need to make sure the age is integer
  # it can be fractional if it was scaled from age_in_months
  x[!is.na(x) & x < 0] <- NA_real_
  as.integer(round_up(x))
}

## Functions for calculating z-scores
MakeZScores1 <-
  function(data, growth_standard, measure, zscore_name, flag_name, flag_max, condition, agevar, sexvar, ...) {

    data[[agevar]] <- round_age_var_for_join(data[[agevar]])

    mutate_call_z <-
      interp(~ ifelse(cond_var == TRUE, round(((x / m)^l - 1) / (s * l), digits = 2L), NA_real_),
        .values = list(x = as.name(measure), agevar = as.name(agevar))
      )

    mutate_call_flag <- interp(~ifelse(abs(zscore_name) > flag_max, 1, 0), zscore_name = as.name(zscore_name))

    data %<>%
      left_join(y = growth_standard, by = setNames(c("age", "sex"), c(agevar, sexvar))) %>%
      mutate(cond_var = ifelse(eval(parse(text = condition)), TRUE, FALSE)) %>%
      mutate_(
        .dots =
          c(
            setNames(list(mutate_call_z), zscore_name),
            setNames(list(mutate_call_flag), flag_name)
          )
      ) %>%
      tbl_df() %>%
      select(-l, -m, -s, -cond_var)
  }

MakeZScores2 <-
  function(data, growth_standard, measure, zscore_name, flag_name, flag_max, flag_min = NULL, condition, agevar, sexvar, oedemavar, ...) {

    data[[agevar]] <- round_age_var_for_join(data[[agevar]])

    mutate_call_z1 <-
      interp(~ ifelse(cond_var == TRUE, ((x / m)^l - 1) / (s * l), NA_real_),
        .values = list(x = as.name(measure), agevar = as.name(agevar), oedemavar = as.name(oedemavar))
      )

    mutate_call_z2 <-
      interp(~ ifelse(z > 3, 3 + ((x - sd3pos) / sd23pos), z),
        .values = list(x = as.name(measure), z = as.name(zscore_name))
      )

    mutate_call_z3 <-
      interp(~ round(ifelse(z < -3, -3 + ((x - sd3neg) / sd23neg), z), digits = 2L),
        .values = list(x = as.name(measure), z = as.name(zscore_name))
      )

    mutate_call_flag <-
      if (is.null(flag_min)) {
        interp(~ifelse(abs(zscore_name) > flag_max, 1, 0), zscore_name = as.name(zscore_name))
      } else {
        interp(~ifelse(zscore_name > flag_max | zscore_name < flag_min, 1, 0), zscore_name = as.name(zscore_name))
      }

    data %<>%
      left_join(y = growth_standard, by = setNames(c("age", "sex"), c(agevar, sexvar))) %>%
      mutate(
        sd3pos = m * ((1 + l * s * 3)^(1 / l)),
        sd23pos = sd3pos - m * ((1 + l * s * 2)^(1 / l)),
        sd3neg = m * ((1 + l * s * -3)^(1 / l)),
        sd23neg = m * ((1 + l * s * -2)^(1 / l)) - sd3neg,
        cond_var = ifelse(eval(parse(text = condition)), TRUE, FALSE)
      ) %>%
      mutate_(
        .dots =
          c(
            setNames(list(mutate_call_z1), zscore_name),
            setNames(list(mutate_call_z2), zscore_name),
            setNames(list(mutate_call_z3), zscore_name),
            setNames(list(mutate_call_flag), flag_name)
          )
      ) %>%
      tbl_df() %>%
      select(-l, -m, -s, -cond_var, -contains("3pos"), -contains("3neg"))

    return(data)
  }


MakeZScores3 <-
  function(data, growth_standard, measure, length_measure, zscore_name, flag_name, flag_max, flag_min = NULL, condition, agevar, sexvar, lenheivar, lenhei_unitvar, oedemavar, ...) {
    mutate_call_z1 <-
      interp(~ ifelse(cond_var == TRUE, ((x / m)^l - 1) / (s * l), NA_real_),
        .values = list(x = as.name(measure), agevar = as.name(agevar), oedemavar = as.name(oedemavar))
      )

    mutate_call_z2 <-
      interp(~ ifelse(z > 3, 3 + ((x - sd3pos) / sd23pos), z),
        .values = list(x = as.name(measure), z = as.name(zscore_name))
      )

    mutate_call_z3 <-
      interp(~ round(ifelse(z < -3, -3 + ((x - sd3neg) / sd23neg), z), digits = 2L),
        .values = list(x = as.name(measure), z = as.name(zscore_name))
      )

    mutate_call_flag <-
      if (is.null(flag_min)) {
        interp(~ifelse(abs(zscore_name) > flag_max, 1, 0), zscore_name = as.name(zscore_name))
      } else {
        interp(~ifelse(zscore_name > flag_max | zscore_name < flag_min, 1, 0), zscore_name = as.name(zscore_name))
      }

    data %<>%
      select(!!c("unique_id", sexvar, agevar, lenheivar, lenhei_unitvar, oedemavar, measure)) %>%
      # very important - merge isn't correct without this!! But may need to check it shouldn't be `trunc` or other...
      mutate_(lenhei_rounded = interp(~round(lenheivar, digits = 1), lenheivar = as.name(lenheivar))) %>%
      left_join(y = growth_standard, by = setNames(c("sex", length_measure), c(sexvar, "lenhei_rounded"))) %>%
      mutate(
        cond_var =
          ifelse((is.na(data[[agevar]]) | (data[[agevar]] >= 0 & data[[agevar]] <= 1856)) & !is_oedema_yes(data[[oedemavar]]), TRUE, FALSE)
      ) %>%
      filter(ifelse(eval(parse(text = condition)), TRUE, FALSE) == TRUE) %>%
      mutate_(
        len_lower = interp(~trunc(lenheivar * 10) / 10, lenheivar = as.name(lenheivar)),
        len_upper = interp(~trunc(lenheivar * 10 + 1) / 10, lenheivar = as.name(lenheivar)),
        len_diff = interp(~(lenheivar - len_lower) / 0.1, lenheivar = as.name(lenheivar))
      ) %>%
      merge(
        y = growth_standard, by.x = c(sexvar, "len_lower"), by.y = c("sex", length_measure),
        all.x = TRUE, suffixes = c("", "_lower")
      ) %>%
      merge(
        y = growth_standard, by.x = c(sexvar, "len_upper"), by.y = c("sex", length_measure),
        all.x = TRUE, suffixes = c("", "_upper")
      ) %>%
      mutate(
        # double-check this with data where len_diff > 0!
        l = ifelse(len_diff > 0, l_lower + len_diff * (l_upper - l_lower), l),
        m = ifelse(len_diff > 0, m_lower + len_diff * (m_upper - m_lower), m),
        s = ifelse(len_diff > 0, s_lower + len_diff * (s_upper - s_lower), s),
        sd3pos = m * ((1 + l * s * 3)^(1 / l)),
        sd23pos = sd3pos - m * ((1 + l * s * 2)^(1 / l)),
        sd3neg = m * ((1 + l * s * -3)^(1 / l)),
        sd23neg = m * ((1 + l * s * -2)^(1 / l)) - sd3neg
      ) %>%
      mutate_(
        .dots =
          c(
            setNames(list(mutate_call_z1), zscore_name),
            setNames(list(mutate_call_z2), zscore_name),
            setNames(list(mutate_call_z3), zscore_name),
            setNames(list(mutate_call_flag), flag_name)
          )
      ) %>%
      select(unique_id, zwfl, zwfl_flag) %>%
      tbl_df()

    return(data)
  }
