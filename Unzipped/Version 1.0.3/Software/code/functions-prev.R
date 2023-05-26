MakeClustStrat <-
  function(df, total = FALSE, var = NULL, poststr_var = NULL, cluster_var = NULL, strata_var = NULL, sw_var = NULL, pretty_name = NULL) {
    df$var <- df[[var]]
    df$poststr_var <- df[[poststr_var]]
    df$cluster_var <- df[[cluster_var]]
    df$strata_var <- df[[strata_var]]
    df$sw_var <- df[[sw_var]]

    # if there is no cluster variable (was designated '1', ids must be '~1')
    if (df$cluster_var %>% unique() %>% length() == 1) {
      if (df$strata_var %>% unique() %>% anyNA()) { # if there are NA values in strata_var
        dstrat <- svydesign(ids = ~1, strata = NULL, weights = ~sw_var, data = df, nest = TRUE)
        dstrat_unw <- svydesign(ids = ~1, strata = NULL, weights = ~1, data = df, nest = TRUE)
      } else {
        dstrat <- svydesign(ids = ~1, strata = ~strata_var, weights = ~sw_var, data = df, nest = TRUE)
        dstrat_unw <- svydesign(ids = ~1, strata = ~strata_var, weights = ~1, data = df, nest = TRUE)
      }
    } else {
      if (df$strata_var %>% unique() %>% anyNA()) { # if there are NA values in strata_var
        dstrat <- svydesign(ids = ~cluster_var, strata = NULL, weights = ~sw_var, data = df, nest = TRUE)
        dstrat_unw <- svydesign(ids = ~cluster_var, strata = NULL, weights = ~1, data = df, nest = TRUE)
      } else {
        dstrat <- svydesign(ids = ~cluster_var, strata = ~strata_var, weights = ~sw_var, data = df, nest = TRUE)
        dstrat_unw <- svydesign(ids = ~cluster_var, strata = ~strata_var, weights = ~1, data = df, nest = TRUE)
      }
    }

    vecn <- svyby(~I(!is.na(var)), ~poststr_var, dstrat, svytotal, drop.empty.groups = FALSE)
    vecn_unw <- svyby(~I(!is.na(var)), ~poststr_var, dstrat_unw, svytotal, drop.empty.groups = FALSE)
    mean_est <- svyby(~var, ~poststr_var, dstrat, svymean, na.rm = TRUE, drop.empty.groups = FALSE)
    mean_est_ci <- svyby(~var, ~poststr_var, dstrat, svyciprop, vartype = "ci", df = degf(dstrat), method = "logit", na.rm.all = TRUE, drop.empty.groups = FALSE)[, 3:4]

    table_est <-
      tibble::tibble(
        "rowname" = paste0(
          pretty_name,
          ": ", rownames(mean_est)
        ),
        "Z_pop" = vecn$`I(!is.na(var))TRUE`, "Z_unwpop" = vecn_unw$`I(!is.na(var))TRUE`,
        "_r" = mean_est$var, "_se" = mean_est$se, "_ll" = mean_est_ci$ci_l, "_ul" = mean_est_ci$ci_u
      )

    if (total == TRUE) {
      vecn <- svytotal(~I(!is.na(var)), dstrat)[2]
      vecn_unw <- svytotal(~I(!is.na(var)), dstrat_unw)[2]
      mean_est <- svymean(~var, dstrat, na.rm = TRUE)
      mean_est_ci <- attr(svyciprop(~var, dstrat, vartype = "ci", method = "logit"), "ci")

      table_est <-
        c("All", vecn, vecn_unw, mean_est, SE(mean_est), mean_est_ci) %>%
        setNames(c("Z_pop", "Z_unwpop", "_r", "_se", "_ll", "_ul")) %>%
        rbind(table_est)
    }

    table_est %<>%
      mutate_at(vars(`_r`:`_ul`), funs(as.numeric(.))) %>%
      mutate_at(vars(`_r`:`_ul`), funs(. * 100))

    return(table_est)
  }


MakeClustStratZ <-
  function(df, total = FALSE, var = NULL, poststr_var = NULL, cluster_var = NULL, strata_var = NULL, sw_var = NULL) {
    df$var <- df[[var]]
    df$poststr_var <- df[[poststr_var]]
    df$cluster_var <- df[[cluster_var]]
    df$strata_var <- df[[strata_var]]
    df$sw_var <- df[[sw_var]]

    # if there is no cluster variable (was designated '1', ids must be '~1')
    if (df$cluster_var %>% unique() %>% length() == 1) {
      if (df$strata_var %>% unique() %>% anyNA()) { # if there are NA values in strata_var
        dstrat <- svydesign(ids = ~1, strata = NULL, weights = ~sw_var, data = df, nest = TRUE)
        dstrat_unw <- svydesign(ids = ~1, strata = NULL, weights = ~1, data = df, nest = TRUE)
      } else {
        dstrat <- svydesign(ids = ~1, strata = ~strata_var, weights = ~sw_var, data = df, nest = TRUE)
        dstrat_unw <- svydesign(ids = ~1, strata = ~strata_var, weights = ~1, data = df, nest = TRUE)
      }
    } else {
      if (df$strata_var %>% unique() %>% anyNA()) { # if there are NA values in strata_var
        dstrat <- svydesign(ids = ~cluster_var, strata = NULL, weights = ~sw_var, data = df, nest = TRUE)
        dstrat_unw <- svydesign(ids = ~cluster_var, strata = NULL, weights = ~1, data = df, nest = TRUE)
      } else {
        dstrat <- svydesign(ids = ~cluster_var, strata = ~strata_var, weights = ~sw_var, data = df, nest = TRUE)
        dstrat_unw <- svydesign(ids = ~cluster_var, strata = ~strata_var, weights = ~1, data = df, nest = TRUE)
      }
    }

    mean_est <- svyby(~var, ~poststr_var, dstrat, svymean, na.rm = TRUE, drop.empty.groups = FALSE)
    mean_est_ci <- confint(mean_est, df = degf(dstrat))

    # the survey package's survey::svyvar fails if
    # there is only one observation with an unexpected error it seems
    # we catch this error here and set all results to NA
    mean_est_sd <- tryCatch({
      svyby(~var, ~poststr_var, dstrat, svyvar,
            na.rm.all = TRUE,
            na.rm = TRUE,
            drop.empty.groups = FALSE)

    }, error = function(er) {
      data.frame(
        dummy = rep.int(NA_real_, nrow(mean_est)),
        result = rep.int(NA_real_, nrow(mean_est))
      )
    })
    table_est <-
      cbind(mean_est[, -c(1)], mean_est_ci, sqrt(mean_est_sd[, c(2)])) %>%
      setNames(c("_r", "_se", "_ll", "_ul", "_stdev"))

    if (total == TRUE) {
      mean_est <- svymean(~var, dstrat, na.rm = TRUE)
      mean_est_ci <- confint(mean_est, df = degf(dstrat))

      table_est <-
        c(mean_est, SE(mean_est), mean_est_ci, sqrt(svyvar(~var, dstrat, na.rm = T))) %>%
        setNames(c("_r", "_se", "_ll", "_ul", "_stdev")) %>%
        rbind(table_est)
    }

    return(table_est)
  }


MakeEstimatesPrev <- function(data, z_prev, z_lab, cutoff, cut_lab, cluster_var, strata_var, sw_var, total, poststr_var, pretty_name, ...) {
  stopifnot(z_prev %in% colnames(data))
  # cutoff is a character that is trusted
  # nevertheless we check for an expected pattern
  stopifnot(nchar(cutoff) <= 9, grepl(pattern = "^[<|>]\\s?[\\-]?[0-9]{1,10}$", x = cutoff))
  data$var_cut <- eval(parse(text = paste0('ifelse(data[["', z_prev, '"]]', cutoff, ", 1, 0)")))

  MakeClustStrat(df = data, total, var = "var_cut", poststr_var, cluster_var, strata_var, sw_var, pretty_name = pretty_name) %>%
    setNames(paste0(z_lab, cut_lab, names(.)))
}

MakeEstimatesZ <- function(data, z_summ, z_lab, cluster_var, strata_var, sw_var, total, poststr_var, ...) {
  stopifnot(z_summ %in% colnames(data))
  data$var_cut <- data[[z_summ]]
  MakeClustStratZ(df = data, total, var = "var_cut", poststr_var, cluster_var, strata_var, sw_var) %>%
    setNames(paste0(z_lab, names(.)))
}


CalculateAllPrevCols <-
  function(data, poststr_var, total = FALSE, cluster_var, strata_var, sw_var, table_z_var, table_cutoff_var, remove, pretty_name, ...) {
    tmp <-
      lapply(1:nrow(table_z_var), function(i)
        lapply(1:nrow(table_cutoff_var), function(j)
          MakeEstimatesPrev(
            data,
            total = total, poststr_var = poststr_var, cluster_var, strata_var, sw_var,
            z_prev = table_z_var[["z_prev"]][i], z_lab = table_z_var[["z_lab"]][i],
            cutoff = table_cutoff_var[["cutoffs"]][j], cut_lab = table_cutoff_var[["cutoffs_lab"]][j],
            pretty_name = pretty_name
          )) %>%
          bind_cols(
            MakeEstimatesZ(data,
              total = total, poststr_var = poststr_var, cluster_var, strata_var, sw_var,
              z_summ = table_z_var[["z_summ"]][i], z_lab = table_z_var[["z_lab"]][i]
            )
          )) %>%
      bind_cols() %>%
      select(-remove) %>%
      setNames(str_replace_all(names(.), "_3Z", "Z")) %>%
      setNames(str_replace_all(names(.), "HA_3r", "r"))


    ## Overlapping stunting and wasting
    data$var_cut <- with(data, ifelse(is.na(zlen) | is.na(zwfl_aux), NA, ifelse(zlen < -2 & zwfl_aux < -2, 1, 0)))

    tmp %<>%
      bind_cols(
        MakeClustStrat(df = data, total = total, var = "var_cut", poststr_var = poststr_var, cluster_var = cluster_var, strata_var = strata_var, sw_var = sw_var) %>%
          select(-1) %>%
          setNames(paste0("HA_2_WH_2", c("_pop", "_unwpop", "_r", "_se", "_ll", "_ul")))
      )

    ## Overlapping stunting and overweight
    data$var_cut <- with(data, ifelse(is.na(zlen) | is.na(zwfl_aux), NA, ifelse(zlen < -2 & zwfl_aux > 2, 1, 0)))

    tmp %<>%
      bind_cols(
        MakeClustStrat(df = data, total = total, var = "var_cut", poststr_var = poststr_var, cluster_var = cluster_var, strata_var = strata_var, sw_var = sw_var) %>%
          select(-c(1:3)) %>%
          setNames(paste0("HA_2_WH2", c("_r", "_se", "_ll", "_ul")))
      )

    return(tmp)
  }

#' Computes the number of missing and excluded cases from the prevalence calculation
#' Only works in a reactive conext.
#'
#' This helper function is used in the prevalence module and in the report.
#'
#' @param dataset a data frame
#' @param list_map_vars a reactive list of variable mappings
anthro_prev_excluded_cases <- function(dataset, list_map_vars) {
  is_none <- function(x) x == "None"
  missing_values_names <- c(
    "sw" = "Sample Weight",
    "cluster" = "Cluster",
    "strata" = "Strata"
  )

  missing_value_counts <- map(names(missing_values_names), function(var) {
    col_name <- list_map_vars[[var]]()
    if (is_none(col_name)) {
      0L
    } else {
      as.integer(sum(is.na(dataset[[col_name]])))
    }
  })

  missing_values_names <- missing_values_names[missing_value_counts > 0L]
  missing_value_counts <- missing_value_counts[missing_value_counts > 0L]
  list(
    missing_values_names = missing_values_names,
    missing_value_counts = missing_value_counts
  )
}
