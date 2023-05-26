# this file contains plot definitions
anthro_ggplot2_style <- function() {
  hrbrthemes::theme_ipsum(
    axis_text_size = 14, axis_title_size = 16,
    base_family = "Palatino", grid = ""
  )
}

anthro_ggplot2_standard_normal <- function(linetype_as_aes = FALSE) {
  mapping <- if (linetype_as_aes) {
    aes(linetype = "WHO standards")
  }
  linetype_arg <- if (!linetype_as_aes) {
    pairlist(linetype = "dotdash")
  }
  rlang::eval_tidy(rlang::quo(
    ggplot2::stat_function(
      mapping = mapping,
      fun = stats::dnorm,
      args = list(mean = 0, sd = 1),
      size = 0.15,
      !!! linetype_arg
    )
  ))
}

anthro_ggplot2_standard_normal_caption <- function() {
  ggplot2::labs(
    caption = "The standard normal density distribution curve is overlaid as a dashed-and-dotted line to provide a visual reference."
  )
}

anthro_scale_linetype_manual_no_groups <- scale_linetype_manual(
  values = c("solid", "dotdash"),
  guide = guide_legend(title = NULL)
)
