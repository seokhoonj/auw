get_demography <- function(cohort, id_var = "id", group_var = c("gender", "age_band")) {
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt  <- env$dt

  id_var    <- instead::capture_names(dt, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(dt, !!rlang::enquo(group_var))

  demo <- dt[, .(n = data.table::uniqueN(.SD)), keyby = group_var,
            .SDcols = id_var]

  data.table::set(demo, j = "prop", value = demo$n / sum(demo$n))

  demo <- env$restore(demo)
  instead::prepend_class(demo, "demo")
}

demo_plot <- function(x, show_label = TRUE,
                      label_args = list(
                        family = getOption("ggshort.font"),
                        size = 4, angle = 90,
                        hjust = 0.5, vjust = 0.5,
                        color = "black"
                      ),
                      theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "demo")
  theme <- match.arg(theme)

  has_gender <- instead::has_cols(x, "gender")

  if (has_gender) {
    p <- ggshort::ggbar(x, x = age_band, y = n, fill = gender) +
      ggshort::scale_fill_gender() +
      ggshort::scale_y_comma() +
      ggshort::switch_theme(theme = theme, ...)
  } else {
    p <- ggshort::ggbar(x, x = age_band, y = n) +
      ggshort::scale_y_comma() +
      ggshort::switch_theme(theme = theme, ...)
  }

  if (show_label) {
    p <- p + ggplot2::geom_text(aes(label = instead::as_comma(n)), vjust = -.25)
  }

  p
}
