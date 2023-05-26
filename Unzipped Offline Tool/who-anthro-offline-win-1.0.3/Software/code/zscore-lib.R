reactive_zscore_validation <- function(dataset, list_map_vars, height_and_weight = FALSE) {
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
    } else if (!is_mapped("sex")) {
      validation_error(
        'You need to provide a mapping for variable "sex"'
      )
    } else if (!height_and_weight && !is_mapped("weight") && !is_mapped("lenhei")) {
      validation_error(
        'You need to provide a mapping for variable "Weight (kg)" or "Length or height (cm)"'
      )
    } else if (height_and_weight && !is_mapped("weight")) {
      validation_error(
        'You need to provide a mapping for variable "Weight (kg)"'
      )
    } else if (height_and_weight && !is_mapped("lenhei")) {
      validation_error(
        'You need to provide a mapping for variable "Length or height (cm)"'
      )
    } else {
      validation_ok()
    }
  })
}
