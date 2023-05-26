# These functions define valid values of data field

valid_sex <- function(x) {
  !all(is.na(x)) && (is.numeric(x) || is.character(x)) &&
    all(x[!is.na(x)] %in% c("1", "M", "m", "2", "F", "f"), na.rm = TRUE)
}

valid_oedema <- function(x) {
  !all(is.na(x)) && (is.character(x) || is.numeric(x)) &&
    all(x[!is.na(x)] %in% c("Y", "y", "N", "n", "1", "2"), na.rm = TRUE)
}

valid_lh <- function(x) {
  !all(is.na(x)) && is.character(x) &&
    all(x[!is.na(x)] %in% c("L", "l", "H", "h"), na.rm = TRUE)
}

valid_wiq <- function(x) {
  valid_vals1 <- as.character(1:5)
  valid_vals2 <- paste0("Q", valid_vals1)
  !all(is.na(x)) && (is.character(x) || is.numeric(x)) &&
    all(x[!is.na(x)] %in% c(valid_vals1, valid_vals2), na.rm = TRUE)
}

valid_numeric <- function(x) {
  !all(is.na(x)) && is.numeric(x)
}

valid_weight <- valid_numeric
valid_height <- valid_numeric
valid_age <- valid_numeric

valid_date <- function(x) {
  lubridate::is.Date(x) || (is.character(x) &&
    !all(is.na(x)) &&
    all(grepl(pattern = "^\\d{1,2}/\\d{1,2}/\\d{4}$", x = x[!is.na(x)]),
      na.rm = TRUE
    ))
}

# valid if at least one value is not NA
valid_always <- function(x) !all(is.na(x))

valid_integerlike <- function(x) {
  valid_always(x) &&
    (
      is.integer(x) ||
        (is.numeric(x) && all(x %% 1 == 0, na.rm = TRUE)) ||
        (is.character(x) &&
          suppressWarnings(sum(is.na(as.integer(x))) == sum(is.na(x))) &&
          suppressWarnings(all(as.integer(x) == x, na.rm = TRUE)))
    )
}
