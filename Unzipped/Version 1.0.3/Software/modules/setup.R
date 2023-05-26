# load modules
# This "module" is not really a module, as it is interwoven with the app.R code
SetUp <- function(input, output, session, panel_main, original_filename) {
  stopifnot(is.reactive(original_filename))
  ns <- session$ns
  output$filename <- renderText(original_filename())

  list_variables_input <-
    list(
      "id_variables_sex", "id_variables_weight", "id_variables_lenhei",
      "id_variables_lenhei_unit", "id_variables_oedema", "id_variables_sw",
      "id_variables_team", "id_variables_cluster", "id_variables_strata",
      "id_variables_typeres", "id_variables_gregion", "id_variables_wealthq",
      "id_variables_mothered", "id_variables_othergr"
    )

  age_popover_title <- "Use date of birth/visit if possible"
  age_popover_content <- 'The best practice is to calculate the exact age of the child based on date of birth and date of visit. In case those are not available, please uncheck the "Compute age" checkbox. Age should then be provided either in days or in unrounded months for best accuracy of z-score calculations. In case age is provided in months, this should be indicated by clicking "Age unit in months?".'
  panel_side <-
    tagList(
      HTML("<b>Filename:</b>"), textOutput(ns("filename")),
      hr(),
      ## Variable mapping
      ### Add checkbox to show/hide list of mapping variables
      checkboxInput("map_var", "Show/hide mapping variables", TRUE),
      hr(),
      conditionalPanel(
        condition = "input.map_var == true",
        list(
          h4("Age mapping", tags$i(
            class = "glyphicon glyphicon-info-sign",
            id = "age-mapping-info-icon",
            title = age_popover_title, `data-toggle` = "popover",
            `data-content` = age_popover_content
          )),
          tags$script(HTML(
            '$(function () { $("#age-mapping-info-icon").popover({"trigger": "hover"})})'
          )),
          checkboxInput("compute_age", "Compute age using Date of birth and Date of visit", TRUE),
          conditionalPanel(
            condition = "input.compute_age == true",
            tagList(
              selectInput("date_format", "Date format", choices = c("dd/mm/yyyy",
                                                                    "mm/dd/yyyy")),
              VariablesInput("id_variables_date_birth"),
              VariablesInput("id_variables_date_obs")
            )
          ),
          conditionalPanel(
            condition = "input.compute_age == false",
            tagList(
              checkboxInput("id_age_unit", "Age unit in months", FALSE),
              VariablesInput("id_variables_age")
            )
          ),
          hr(),
          h4("Other variables"),
          lapply(list_variables_input, VariablesInput)
        )
      ),

      ## Filter modules
      hr(),
      h4("Data filter"),
      VariablesInput("id_variables_filter"),
      uiOutput("ui_filter"), # column filters
      actionButton("apply_filter", "Apply filters") # filter button

    )
  reactive(list(
    side = panel_side,
    main = panel_main
  ))
}
