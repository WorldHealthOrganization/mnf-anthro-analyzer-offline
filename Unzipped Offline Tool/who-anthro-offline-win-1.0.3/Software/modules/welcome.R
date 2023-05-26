Welcome <- function(input, output, session) {
  HTML(
    paste(
      "Welcome to the Anthro Survey Analyser.",
      "Please upload your dataset using the Upload Data button",
      img(src = "arrow.png", width = "5%"),
      sep = "<br/>"
    )
  )
}
