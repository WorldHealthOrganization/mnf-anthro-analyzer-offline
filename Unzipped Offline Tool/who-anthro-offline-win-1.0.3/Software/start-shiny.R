# Script that starts the shiny webserver
# Parameters are supplied using envirnoment variables
lib_path <- normalizePath("R/library")
assign(".lib.loc", lib_path, envir = environment(.libPaths))
Sys.setenv(ANTHRO_IS_EMBEDDED = "1")
Sys.setenv(RSTUDIO_PANDOC = normalizePath("pandoc"))
suppressMessages(
  suppressWarnings(
    shiny::runApp(
      "Software/.",
      launch.browser = TRUE
    )
  )
)
