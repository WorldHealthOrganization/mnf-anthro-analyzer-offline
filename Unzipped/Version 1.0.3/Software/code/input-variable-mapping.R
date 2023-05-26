# generates the variables supported in the app, their labels and validations
anthro_input_variable_mapping <- function() {
  tibble::tibble(
    vars = c(
      "age",
      "date_birth",
      "date_obs",
      "sex",
      "weight",
      "lenhei",
      "lenhei_unit",
      "oedema",
      "sw",
      "team",
      "cluster",
      "strata",
      "wealthq",
      "typeres",
      "gregion",
      "mothered",
      "othergr"
    ),
    labs =
      c(
        "Age",
        "Date of birth",
        "Date of visit",
        "Sex <br/> (Male = 1/m/M; <br/> Female = 2/f/F)",
        "Weight (kg)",
        "Length or height (cm)",
        "LH measure",
        "Oedema",
        "Sampling Weight",
        "Team",
        "Cluster",
        "Strata",
        "Wealth quintile",
        "Residence type",
        "Geographical region",
        "Mother education",
        "Other grouping variable"
      ),

    # A list of function to define valid columns
    validators = list(
      valid_age,
      valid_date,
      valid_date,
      valid_sex,
      valid_weight,
      valid_height,
      valid_lh,
      valid_oedema,
      valid_always,
      valid_always,
      valid_integerlike,
      valid_integerlike,
      valid_wiq,
      valid_always,
      valid_always,
      valid_always,
      valid_always
    ),

    validator_explanations = c(
      list(
        'Candidates for "Age" must be numeric and not all values can be missings.',
        'Candidates for "Date of birth" must have the format "dd/mm/yyyy" or "mm/dd/yyyy" and not all values can be missings.',
        'Candidates for "Date of visit" must have the format "dd/mm/yyyy" or "mm/dd/yyyy" and not all values can be missings.',
        'Candidates for "Sex" must have the format "(Male = 1/m/M; Female = 2/f/F)" and not all values can be missings.',
        'Candidates for "Weight (kg)" must be numeric and not all values can be missings.',
        'Candidates for "Length or height (cm)" must be numeric and not all values can be missings.',
        'Candidates for "LH measure" must have one of these values ("L", "l", "H", "h") and not all values can be missings.',
        'Candidates for "Oedema" must have one of these values ("Y", "y", "N", "n", "1", "2") and not all values can be missings.',
        'Candidates for "Sampling Weight" must have at least one non missing value.',
        'Candidates for "Team" must have at least one non missing value.',
        'Candidates for "Cluster" must be integers (no decimal points) and not all values can be missings.',
        'Candidates for "Strata" must be integers (no decimal points) and not all values can be missings.',
        'Candidates for "Wealth quintile" must be integers (1, 2, 3, 4, 5) or characters ("Q1", "Q2", "Q3", "Q4", "Q5") and not all values can be missings.'
      ),
      lapply(
        c(
          "Residence type",
          "Geographical region",
          "Mother education",
          "Other grouping variable"
        ),
        function(x)
          paste0('Candidates for "', x, '" must have at least one non missing value.')
      )
    ),
    stringsAsFactors = FALSE
  )
}
