#' Risk rate plot
#'
#' Draw line plots of risk rates by `age` and `gender`, faceted by `risk_nm`.
#' Optionally annotate each facet with the maximum rate (and the `age` at which
#' it occurs). Colors are provided by `ggshort::scale_color_pair_manual()`.
#'
#' @param risk_info A data frame containing at least the columns `risk_nm`,
#'   `gender` (factor), `age` (numeric), and `rate` (numeric).
#' @param x Character vector of risk names to include (matching `risk_nm`).
#'   If omitted, the function interactively prompts. Use `"all"` to include all.
#' @param logscale Logical; if `TRUE`, use a log10 y-scale. Otherwise linear.
#' @param max_label Logical; if `TRUE`, draw labels for per-gender maxima within
#'   each `risk_nm` facet (showing `max rate (age)`).
#' @param age_interval Integer; controls the number of x-axis breaks by
#'   `floor(uniqueN(age) / age_interval)`. Default is `10`.
#' @param nrow,ncol Number of rows/columns passed to `ggplot2::facet_wrap()`.
#' @param scales Facet scales: `"fixed"` (default), `"free"`, `"free_x"`,
#'   or `"free_y"`. Passed to `facet_wrap()`.
#' @param theme One of `"view"`, `"save"`, or `"shiny"`; forwarded to
#'   `ggshort::switch_theme()`.
#' @param ... Additional arguments forwarded to `ggshort::switch_theme()`
#'   (e.g., `theme_view`, `theme_save`, `theme_shiny` options).
#'
#' @return A `ggplot` object.
#'
#' @details
#' - When `max_label = TRUE`, per-facet labels are assembled and drawn using
#'   `geom_label()` with `family = getOption("ggshort.font")`.
#' - If the maximum of `rate` is `<= 1`, y-axis labels are formatted as percent;
#'   otherwise shown on the raw scale (or log-scale if `logscale = TRUE`).
#'
#' @examples
#' \dontrun{
#' # Basic usage (all risks)
#' risk_plot(risk_info, x = "all")
#'
#' # Specific risks, log scale, custom facet layout
#' risk_plot(risk_info,
#'           x = c("risk rate 1", "risk rate 2"),
#'           logscale = TRUE, nrow = 1, scales = "free_y")
#' }
#'
#' @export
risk_plot <- function(risk_info, x, logscale = FALSE, max_label = TRUE,
                      age_interval = 10,
                      nrow = NULL, ncol = NULL, scales = "fixed",
                      theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(risk_info, "data.frame")
  instead::assert_class(risk_info$gender, "factor")
  theme <- match.arg(theme)

  env <- instead::ensure_dt_env(risk_info)
  dt  <- env$dt

  age <- gender <- label <- max_rate <- rate <- risk <- NULL
  if (missing(x)) {
    instead::dprint(unique(dt[, .SD, .SDcols = "risk_nm"]))
    x <- trimws(strsplit(
      readline("Please insert risk (if you want all risks, 'all'): "),
      split = "\\|")[[1L]])
  }
  if (x != "all") dt <- dt[dt$risk_nm %chin% x]

  dt_s <- dt[!is.na(rate), .(max_rate = max(.SD)), .(risk_nm, gender),
             .SDcols = "rate"]
  dt[dt_s, `:=`(max_rate, max_rate), on = .(risk_nm, gender, rate = max_rate)]
  dt_a <- dt[!is.na(max_rate), .(age = min(age)), .(risk_nm, gender, max_rate)]
  data.table::setcolorder(dt_a, "age", after = "gender")
  instead::rm_cols(dt, max_rate)
  dt[dt_a, on = .(risk_nm, gender, age), `:=`(max_rate, max_rate)]
  dt[, `:=`(label, ifelse(!is.na(max_rate), sprintf("%.4f (%d)", max_rate, age), max_rate))]
  dt_a[, `:=`(label, sprintf("%d: %.4f (%d)", gender, max_rate, age)), .(risk_nm)]
  dt_b <- dt_a[, .(label = instead::paste_uni_str(label, "\n")), .(risk_nm)]
  dt_b[, `:=`(gender, factor(1, levels = c(1, 2)))]
  dt_b[, `:=`(age, -Inf)]
  dt_b[, `:=`(rate, Inf)]

  scale_y_fun <- if (logscale) ggplot2::scale_y_log10 else ggplot2::scale_y_continuous

  p <- ggshort::ggline(
    data = dt,
    x = .data[["age"]], y = .data[["rate"]],
    color = .data[["gender"]]
  ) +
    list(if (max_label) {
      ggplot2::geom_label(data = dt_b, aes(label = .data[["label"]]),
                          family = getOption("ggshort.font"),
                          colour = "black", alpha = .3,
                          hjust = -.1, vjust = 1.2)
    }) +
    ggshort::scale_color_gender() +
    ggplot2::scale_x_continuous(
      n.breaks = floor(instead::unilen(dt$age)/age_interval)
    ) +
    scale_y_fun(labels = function(x) {
    if (max(dt$rate, na.rm = TRUE) <= 1) {
      sprintf("%.2f%%", x * 100)
    } else {
      sprintf("%.2f", x)
    }}) +
    ggplot2::facet_wrap(~ risk_nm, nrow = nrow, ncol = ncol, scales = scales) +
    ggplot2::ylab(if (!logscale) "rate" else "log(rate)") +
    ggshort::switch_theme(theme = theme, ...)

  p
}

#' Compare two risk rates
#'
#' Plot two risk-rate series side-by-side (by `gender`) and their pointwise ratio
#' (`risk_x / risk_y`) with reference lines. A shared legend is placed below,
#' and an overall title like `Risk Rate Ratio = risk_x / risk_y` is added.
#'
#' @param risk_info A data frame containing at least the columns `risk_nm`,
#'   `gender` (factor), `age` (numeric), and `rate` (numeric).
#' @param x,y Character scalars; the two `risk_nm` values to compare.
#'   If omitted, the function interactively prompts for each.
#' @param age_unit Integer; controls the number of breaks on the x-axis via
#'   `floor(uniqueN(age) / age_unit)`. Default is `10`.
#' @param logscale Logical; if `TRUE`, use a log10 y-scale for both panels.
#' @param scales Facet scales: `"fixed"` (default), `"free"`, `"free_x"`,
#'   or `"free_y"`. Passed to `facet_wrap()`.
#' @param nrow,ncol Number of rows/columns for faceting (used in the rate panel).
#' @param heights Numeric length-2 vector giving the relative heights for the
#'   stacked layout (top panels vs. legend). Default `c(8.5, 1.5)`.
#' @param widths Numeric length-2 vector giving the relative widths for the
#'   side-by-side panels (rate plot vs. ratio plot). Default `c(5, 3)`.
#' @param theme One of `"view"`, `"save"`, or `"shiny"`; forwarded to
#'   `ggshort::switch_theme()`.
#' @param ... Additional arguments forwarded to `ggshort::switch_theme()`.
#'
#' @return A `ggplot` object composed from grobs (legend extracted and stacked).
#'   The raw merged data (`dz`) is attached as `attr(p, "raw")`.
#'
#' @details
#' - The left panel shows two risk-rate lines (colored by `gender`, linetype by risk),
#'   faceted by `gender`. Colors use `ggshort::scale_color_pair_manual()`.
#' - The right panel shows the pointwise ratio `rate_x / rate_y` by `age`,
#'   with a horizontal line at 1 (`geom_hline1()`) and a mean line
#'   (`stat_mean_hline()`), faceted by risk label.
#' - The plot title is constructed via `bquote("Risk Rate Ratio = " * frac(.(x), .(y)))`.
#'
#' @examples
#' \dontrun{
#' # Compare two risks with default theme
#' comp_risk_plot(risk_info, x = "risk rate 1", y = "risk rate 2")
#'
#' # Free y-scales and log scale for the ratio panel
#' comp_risk_plot(risk_info, x = "risk rate 1", y = "risk rate 2",
#'                logscale = TRUE, scales = "free_y")
#' }
#'
#' @export
comp_risk_plot <- function(risk_info, x, y,
                           age_unit = 10,
                           logscale = FALSE,
                           scales = "fixed", nrow = NULL, ncol = NULL,
                           heights = c(8.5, 1.5), widths = c(5, 3),
                           theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(risk_info, "data.frame")

  env <- instead::ensure_dt_env(risk_info)
  dt  <- env$dt

  if (missing(x) && missing(y)) {
    instead::dprint(unique(dt[, .SD, .SDcols = "risk_nm"]))
    x <- trimws(strsplit(
      readline("Please insert 1st risk: "),
      split = "\\|")[[1L]]
    )
    y <- trimws(strsplit(
      readline("Please insert 2nd risk: "),
      split = "\\|")[[1L]]
    )
  } else if (missing(x) && !missing(y)) {
    instead::dprint(unique(dt[, .SD, .SDcols = "risk_nm"]))
    x <- trimws(strsplit(
      readline("Please insert 1st risk: "),
      split = "\\|")[[1L]]
    )
  } else if (!missing(x) && missing(y)) {
    instead::dprint(unique(dt[, .SD, .SDcols = "risk_nm"]))
    y <- trimws(strsplit(
      readline("Please insert 2nd risk: "),
      split = "\\|")[[1L]]
    )
  }

  dx <- dt[risk_nm == x]
  dy <- dt[risk_nm == y]

  by <- cols <- id.vars <- c("gender", "age")
  dz <- merge(dx, dy, by = by, all.x = TRUE)

  data.table::setorderv(dz, cols)
  data.table::setnames(dz, gsub("\\.", "_", names(dz)))
  data.table::set(dz, j = "rate_x_prop", value = dz$rate_x/(dz$rate_x + dz$rate_y))
  data.table::set(dz, j = "rate_y_prop", value = dz$rate_y/(dz$rate_x + dz$rate_y))
  data.table::set(dz, j = "ratio", value = dz$rate_x/dz$rate_y)

  dm <- data.table::melt(
    dz, id.vars = id.vars, measure.vars = c("rate_x", "rate_y", "ratio"),
    variable.name = c("risk"), value.name = c("rate")
  )
  data.table::set(dm, i = which(dm$risk == "rate_x"), j = "risk", value = risk1)
  data.table::set(dm, i = which(dm$risk == "rate_y"), j = "risk", value = risk2)
  data.table::set(dm, j = "label", value = paste(dm$risk, "(", dm$gender, ")"))

  # for p1
  scale_y_fun <- if (logscale) ggplot2::scale_y_log10 else ggplot2::scale_y_continuous
  title       <- bquote("Risk Rate Ratio = " * frac(.(risk1), .(risk2)))

  # rate
  data_rate <- dm[risk != "ratio"]
  p1 <- ggshort::ggline(
    data = data_rate,
    x = .data[["age"]], y = .data[["rate"]],
    color = .data[["gender"]], group = .data[["label"]],
    linetype = .data[["risk"]]
  ) +
    ggplot2::scale_x_continuous(
      n.breaks = floor(instead::unilen(data_rate$age) / age_unit)
    ) +
    scale_y_fun(labels = function(x) {
    if (max(data_rate$rate, na.rm = TRUE) <= 1) {
      sprintf("%.2f%%", x * 100)
    } else {
      sprintf("%.2f", x)
    }}) +
    ggshort::scale_color_gender() +
    ggplot2::facet_wrap("~ gender", scales = scales) +
    ggplot2::theme(legend.box = "horizontal") +
    ggshort::switch_theme(theme = theme)

  # Ratio
  data_ratio <- dm[risk == "ratio"]
  p2 <- ggshort::ggline(
    data = data_ratio,
    x = .data[["age"]], y = .data[["rate"]],
    color = .data[["gender"]], group = .data[["gender"]]
  ) +
    ggshort::geom_hline1() +
    ggshort::stat_mean_hline() +
    ggplot2::scale_x_continuous(n.breaks = floor(instead::unilen(data_ratio$age) / age_unit)) +
    scale_y_fun(n.breaks = floor(unilen(data_ratio$age)/age_unit)) +
    ggshort::scale_color_gender() +
    ggplot2::ylab("ratio") +
    ggplot2::facet_wrap("~ risk") +
    ggshort::switch_theme(theme = theme)

  legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position = "none")

  p <- hstack_plots(p1, p2, widths = c(6, 4))
  p <- vstack_plots(p, legend, heights = c(8.5, 1.5))
  p <- p |> add_title(title = title)

  p <- grob_to_ggplot(p)
  data.table::setattr(p, "raw", env$restore(dz))

  p
}
