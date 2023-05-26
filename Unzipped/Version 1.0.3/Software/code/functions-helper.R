#' Groups age in months into groups
#' @param age_in_months numeric vector of positive ages
anthro_age_groups <- function(age_in_months) {
  cut_breaks <- c(0, 6, 12, 24, 36, 48, 60)
  cut_labels <- c("00-05 mo", "06-11 mo", "12-23 mo", "24-35 mo", "36-47 mo", "48-59 mo")
  cut(age_in_months, breaks = cut_breaks, labels = cut_labels, right = FALSE)
}

#' banker's rounding for 0 digits and positive numerics
#' i.e. < .5 down, >= .5 up
#' from the anthro R package
round_up <- function(x) {
  stopifnot(is.numeric(x), all(x >= 0, na.rm = TRUE))
  if (length(x) == 0) {
    return(numeric())
  }
  x_rounded <- floor(x)
  rest <- x - x_rounded
  rounded_up <- rest >= 0.5
  rounded_up[is.na(rounded_up)] <- FALSE
  x_rounded[rounded_up] <- x_rounded[rounded_up] + 1
  x_rounded
}

is_oedema_yes <- function(x) {
  tolower(trimws(x)) %in% c("y", "1")
}

is_measure_height <- function(x) trimws(tolower(x)) == "h"
is_measure_length <- function(x) trimws(tolower(x)) == "l"

is_variable_mapped <- function(x) {
  stopifnot(is.character(x), length(x) == 1L)
  tolower(x) != "none"
}

is_variable_not_mapped <- function(x) {
  !is_variable_mapped(x)
}

parse_date <- function(x, format = "dd/mm/yyyy") {
  if (!is.character(format) || length(format) != 1L) {
    format <- "dd/mm/yyyy"
  }
  if (format == "dd/mm/yyyy") {
    lubridate::dmy(x, quiet = TRUE)
  } else if (format == "mm/dd/yyyy") {
    lubridate::mdy(x, quiet = TRUE)
  } else {
    rep.int(as.Date(NA_character_), length(x))
  }
}

# the survey package fails, if a cluster has more than 1 strata
# this helper function checks if that is the case
are_there_non_nested_clusters <- function(dataset, list_map_vars) {
  stopifnot(is.reactive(dataset))
  stopifnot(is.list(list_map_vars))
  stopifnot(is.reactive(list_map_vars[["cluster"]]))
  stopifnot(is.reactive(list_map_vars[["strata"]]))
  if (list_map_vars[["cluster"]]() == "None" ||
    list_map_vars[["strata"]]() == "None") {
    return(FALSE)
  }
  cluster_col <- list_map_vars[["cluster"]]()
  strata_col <- list_map_vars[["strata"]]()
  data <- dplyr::group_by(dataset(), !!as.name(cluster_col))
  data <- dplyr::filter(data, dplyr::n_distinct(!!as.name(strata_col)) > 1L)
  nrow(data) >= 1L
}

# Creates labels for plots that use the "Other grouping" variable
anthro_make_grouping_label <- function(other_grouping_name) {
  if (!is.character(other_grouping_name) ||
    length(other_grouping_name) != 1L ||
    other_grouping_name == "None") {
    "Other\ngrouping"
  } else {
    paste0(strwrap(other_grouping_name, 10L), collapse = "\n")
  }
}

# creates a reactive element that checks if a dataset is valid for
# prevalence calculations
anthro_create_prevalence_validation <- function(list_map_vars, dataset) {
  stopifnot(shiny::is.reactive(dataset), is.list(list_map_vars))
  reactive({
    is_mapped <- function(key) {
      val <- list_map_vars[[key]]()
      !is.null(val) && val != "None"
    }

    if (nrow(dataset()) == 0L) {
      validation_error(
        "The dataset does not have any rows."
      )
    } else if (any(!c("age_group", "age_in_days") %in% colnames(dataset()))) {
      validation_error(
        "You have not provided a mapping for the age yet."
      )
    } else if (list_map_vars[["sex"]]() == "None") {
      validation_error(
        'You need to provide a mapping for variable "sex"'
      )
    }
    else if (list_map_vars[["weight"]]() == "None") {
      validation_error(
        'You need to provide a mapping for variable "Weight"'
      )
    } else if (list_map_vars[["lenhei"]]() == "None") {
      validation_error(
        'You need to provide a mapping for variable "Length or Height"'
      )
    } else {
      validation_ok()
    }
  })
}

make_export_filename <- function(base_name, extension) {
  paste0(base_name, "_", extension)
}

list_mapping_to_char <- function(list_map_vars) {
  res <- lapply(list_map_vars, function(x) x())
  purrr::discard(res, is.null)
}

recode_sex_factor <- function(sex) {
  stopifnot(is.factor(sex))
  # fct_collapse warns if not all rhs factors are present
  # since this will never happen we suppress the warnings
  suppressWarnings(forcats::fct_collapse(
    sex,
    Male = c("1", "M", "m", "male"),
    Female = c("2", "F", "f", "female")
  ))
}

anthro_read_uploaded_csv <- function(path) {
  tryCatch({
    possible_encodings <- readr::guess_encoding(path)
    shiny::req(is.data.frame(possible_encodings))
    possible_encodings <-
      dplyr::arrange(possible_encodings, desc(confidence))
    file_encoding <- if (nrow(possible_encodings) >= 1L) {
      possible_encodings[["encoding"]][1L]
    } else {
      ""
    }
    res <- try(suppressMessages(read.csv(path,
      fileEncoding = file_encoding,
      na.strings = c("NA", ""),
      stringsAsFactors = FALSE
    )))
    if (is.data.frame(res)) {
      res <- tibble::as.tibble(res)
      res <- dplyr::mutate_if(res, is.character, trimws)
      res <- dplyr::mutate_if(res, is.character, enc2native)
      res <- dplyr::mutate_if(res, is.character, function(x) {
        x[x == ""] <- NA_character_
        x
      })
      res
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

recode_wiq_factor <- function(wiqs) {
  stopifnot(is.factor(wiqs))
  # fct_collapse warns if not all rhs factors are present
  # since this will never happen we suppress the warnings
  suppressWarnings(forcats::fct_collapse(
    wiqs,
    `Q1: Poorest` = c("1", "Q1"),
    `Q2` = c("2", "Q2"),
    `Q3` = c("3", "Q3"),
    `Q4` = c("4", "Q4"),
    `Q5: Richest` = c("5", "Q5")
  ))
}

is_label_wiq <- function(x) {
  grepl(x = x, pattern = "^Wealth.*quintile$")
}
