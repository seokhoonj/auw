#' Face amount mix plot
#'
#' Draw a face amount mix plot.
#'
#' @param amt_mix a data.frame specifying a face amount mix by gender, age_band
#' @param label_size a string specifying label size
#' @param theme a string specifying a [match_theme()] function ("view", "save", "shiny")
#' @return a gtable object
#'
#' @export
amt_plot <- function(amt_mix, label_size = 4,
                     theme = c("view", "save", "shiny")) {
# to be updated -----------------------------------------------------------

  jaid::has_cols(
    amt_mix, c("gender", "age_band", "amt", "n", "nsum", "prop"),
    error_raise = TRUE
  )
  age_band <- amt <- gender <- label <- nsum <- prop <- NULL
  amt_mix_uni <- unique(amt_mix[, .(gender, age_band, nsum)])
  amt_mix_uni[, label := jaid::as_comma(nsum)]
  ymax <- max(amt_mix_uni$n * 1.1)
  width <- nchar(max(amt_mix$amt))
  theme <- match.arg(theme)
  g1 <- ggbar(amt_mix_uni, x = age_band, y = nsum, ymax = ymax * 1.1,
              group = gender, fill = gender, label = label,
              label_size = label_size, label_hjust = -.1) +
    scale_pair_fill_manual(amt_mix_uni$gender) +
    scale_y_comma() +
    coord_flip() +
    facet_wrap(~ gender, ncol = 1) + xlab("age band") +
    match_theme(theme = theme, x.size = 0, legend.position = "none")
  g2 <- ggmix(amt_mix, x = age_band, y = prop, group = amt,
              fill = amt, label = label, label_size = label_size) +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", labels = function(x)
      stringr::str_pad(format(x, big.mark = ",", scientific = F), width = width)) +
    coord_flip() + facet_wrap(~ gender, ncol = 1) + xlab("") + theme_test() +
    match_theme(x.size = 0, x.angle = 90, legend.position = "right")
  legend <- get_legend(g2)
  g2 <- g2 + match_theme(theme = theme, x.size = 0, legend.position = "none")
  p <- grid_left_to_right(g1, g2, legend)
  gridExtra::grid.arrange(p)
}
