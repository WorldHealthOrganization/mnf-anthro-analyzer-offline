anthro_UI <- function() {
  dependent_pkgs <- readRDS(file = "static/dependent-packages.rds")
  dependent_pkgs <- setdiff(dependent_pkgs, c("tools", "parallel", "shiny"))
  list(
    shinyjs::useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", href = "style.css")),
    navbarPage(
      title = a(
        href = "http://www.who.int/nutrition/en/",
        target = "_blank",
        shiny::includeHTML("static/who-logo.svg")
      ),
      id = "mainNavBar",
      windowTitle = "WHO Anthro Survey Analyser",
      collapsible = TRUE,
      inverse = TRUE,
      tabPanel("Home",
               shiny::includeHTML("static/home.html"),
               div(class = "row",
                   div(class = "col-md-6 col-md-offset-3 text-center",
                       hr(),
                       actionButton("gotoanthro",
                                    class = "btn btn-success",
                                    label = "Go to the Anthro Survey Analyser"))),
               p("")),
      tabPanel(
        "Analyser",
        value = "analyser",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            id = "nav-sidebar",
            UploadInput("tmp"),
            uiOutput("panel_side")
          ),
          mainPanel(
            width = 10,
            uiOutput("panel_main")
          )
        )
      ),
      navbarMenu(
        "About",
        tabPanel(
          "FAQs and Quick Guide",
          shiny::includeHTML("static/user-manual.html")
        ),
        tabPanel(
          "Software",
          div(
            class = "row",
            div(
              class = "col-md-6 col-md-offset-3",
              h1("Software"),
              p(
                "The application was developed using ",
                a(href = "https://www.r-project.org/", "R: A Language and Environment for Statistical Computing"),
                "and ",
                a(href = "https://cran.r-project.org/package=shiny", "shiny: Web Application Framework for R"),
                ". Additional R packages used to support the application include:"
              ),
              do.call(
                tags$ul,
                lapply(dependent_pkgs, function(pkg) {
                  tags$li(a(href = paste0(
                    "https://cran.r-project.org/package=", pkg
                  ), target = "_blank", pkg))
                })
              )
            )
          )
        ),
        tabPanel(
          "License",
          shiny::includeHTML("static/license.html")
        ),
        tabPanel(
          "Feedback",
          shiny::includeHTML("static/feedback.html")
        )
      )
    )
  )
}
