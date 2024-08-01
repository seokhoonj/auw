#' Risk rate plot
#'
#' Draw a risk rate plot.
#'
#' @param risk_info a data frame specifying risk_rate
#' @param x a string vector specifying risk
#' @param logscale a boolean specifying a log scale
#' @param max_label a boolean whether to draw a max label or not
#' @param label_family a string specifying label font-family
#' @param age_interval a numeric specifying an age interval for the x label (default: 10)
#' @param nrow,ncol Number of rows and columns.
#' @param scales Should `scales` be fixed (`"fixed"`, the default), free (`"free"`), or free in one
#' dimension (`"free_x"`, `"free_y"`)?
#' @param theme a string specifying a [match_theme()] function ("view", "save", "shiny")
#' @param ... arguments passed on to (theme_view, theme_save, theme_shiny)
#' @return a ggplot object
#'
#' @examples
#' # draw a risk rate plot
#' \dontrun{
#' risk_plot(risk_info)}
#'
#' @export
risk_plot <- function(risk_info, x, logscale = FALSE, max_label = TRUE,
                      label_family = "Comic Sans MS", age_interval = 10,
                      nrow = NULL, ncol = NULL, scales = "fixed",
                      theme = c("view", "save", "shiny"), ...) {
  jaid::assert_class(risk_info, "data.frame")
  jaid::assert_class(risk_info$gender, "factor")
  old_class <- class(risk_info)
  jaid::set_dt(risk_info)
  theme <- match.arg(theme)
  age <- gender <- label <- max_rate <- rate <- risk <- NULL
  if (missing(x)) {
    hprint(unique(risk_info[, .SD, .SDcols = "risk"]))
    x <- trimws(strsplit(
      readline("Please insert risk (if you want all risks, 'all'): "),
      split = "\\|")[[1L]]
    )
  }
  if (any(x == "all")) {
    risk_info <- risk_info[risk_info$grade <= 1,]
  } else {
    risk_info <- risk_info[risk_info$risk %chin% x,]
  }
  risk_info_s <- risk_info[!is.na(rate), .(max_rate = max(.SD)), .(risk, gender),
                           .SDcols = "rate"]
  risk_info[risk_info_s, `:=`(max_rate, max_rate),
            on = .(risk, gender, rate = max_rate)]
  risk_info_a <- risk_info[!is.na(max_rate), .(age = min(age)),
                           .(risk, gender, max_rate)]
  data.table::setcolorder(risk_info_a, "age", after = "gender")
  rm_cols(risk_info, max_rate)
  risk_info[risk_info_a, on = .(risk, gender, age), `:=`(max_rate, max_rate)]
  risk_info[, `:=`(label, ifelse(!is.na(max_rate), sprintf("%.4f (%d)", max_rate, age), max_rate))]
  risk_info_a[, `:=`(label, sprintf("%d: %.4f (%d)", gender, max_rate, age)), .(risk)]
  risk_info_b <- risk_info_a[, .(label = jaid::paste_uni_str(label, "\n")), .(risk)]
  risk_info_b[, `:=`(gender, factor(1, levels = c(1, 2)))]
  risk_info_b[, `:=`(age, -Inf)]
  risk_info_b[, `:=`(rate, Inf)]

  scale_y_fun <- if (logscale) scale_y_log10 else scale_y_continuous
  g <- ggline(risk_info, x = age, y = rate, color = gender) +
    list(if (max_label) {
      geom_label(data = risk_info_b, aes(label = label),
                 family = label_family, colour = "black", alpha = .3,
                 hjust = -.1, vjust = 1.2)
    }) +
    scale_pair_color_manual(risk_info$gender) +
    scale_x_continuous(n.breaks = floor(jaid::unilen(risk_info$age)/age_interval)) +
    scale_y_fun(labels = function(x) {
    if (max(risk_info$rate, na.rm = TRUE) <= 1) {
      sprintf("%.2f%%", x * 100)
    } else {
      sprintf("%.2f", x)
    }}) +
    facet_wrap(~ risk, nrow = nrow, ncol = ncol, scales = scales) +
    ylab(if (!logscale) "rate" else "log(rate)") +
    match_theme(theme = theme, ...)

  jaid::set_attr(risk_info, "class", old_class)
  return(g)
}

#' Compare two risk rates
#'
#' Compare two risk rates.
#'
#' @param risk_info a data frame specifying risk_rate
#' @param risk1 a string specifying risk 1
#' @param risk2 a string specifying risk 2
#' @param plot a logical whether to show a plot or not
#' @param nrow,ncol Number of rows and columns.
#' @param logscale a logical whether to use a log scale
#' @param scales Should scales be fixed ("`fixed`", the default), free ("`free`"),
#' or free in one dimension ("`free_x`","`free_y`")?
#' @param age_unit age interval unit to express
#' @param widths a unit vector giving the height of each row or the widths of 3 columns
#' (risk rate plot, ratio plot, legend)
#' @param heights a unit vector giving the height of 2 row (top and plot)
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @param ... arguments passed on to (theme_view, theme_save, theme_shiny)
#' @return no return values
#'
#' @examples
#' # compare two risk rates
#' \dontrun{
#' comp_risk_plot(risk_info, risk1, risk2)}
#'
#' @export
comp_risk_plot <- function(risk_info, risk1, risk2, plot = TRUE,
                           logscale = FALSE, nrow = NULL, ncol = NULL,
                           scales = "fixed", age_unit = 10,
                           widths = c(5, 3, 2), heights = c(2, 8),
                           theme = c("view", "save", "shiny"), ...) {
  x <- risk_info[risk_info$risk == risk1,]
  y <- risk_info[risk_info$risk == risk2,]
  z <- merge(x, y, by = c("gender", "age"), all.x = TRUE)
  data.table::setorder(z, gender, age)
  data.table::setnames(z, gsub("\\.", "_", names(z)))
  data.table::set(z, j = "rate_x_prop", value = z$rate_x/(z$rate_x + z$rate_y))
  data.table::set(z, j = "rate_y_prop", value = z$rate_y/(z$rate_x + z$rate_y))
  data.table::set(z, j = "ratio", value = z$rate_x/z$rate_y)
  m <- data.table::melt(z, id.vars = c("age", "gender"),
                        measure.vars = c("rate_x", "rate_y", "ratio"),
                        variable.name = c("risk"), value.name = c("rate"))
  data.table::set(m, i = which(m$risk == "rate_x"), j = "risk", value = risk1)
  data.table::set(m, i = which(m$risk == "rate_y"), j = "risk", value = risk2)
  data.table::set(m, j = "label", value = paste(m$risk, "(", m$gender, ")"))
  age <- gender <- label <- rate <- risk <- NULL
  if (plot) {
    scale_y_fun <- if (logscale) scale_y_log10 else scale_y_continuous
    mn <- m[m$risk != "ratio",]
    g1 <- ggline(mn, x = age, y = rate, group = label, color = gender,
                 linetype = risk) +
      scale_x_continuous(n.breaks = floor(jaid::unilen(mn$age) / age_unit)) +
      scale_y_fun(labels = function(x) {
      if (max(mn$rate, na.rm = TRUE) <= 1) {
        sprintf("%.2f%%", x * 100)
      } else {
        sprintf("%.2f", x)
      }}) +
      scale_pair_color_manual(c(1, 2)) +
      facet_wrap(~ gender, scales = scales) +
      match_theme(theme = theme, ...)
    mr <- m[m$risk == "ratio",]
    g2 <- ggline(mr, x = age, y = rate, group = gender,
                 color = gender) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      stat_mean_hline(aes(group = gender, color = gender)) +
      scale_x_continuous(n.breaks = floor(jaid::unilen(mn$age) / age_unit)) +
      scale_y_fun(n.breaks = floor(unilen(mr$age)/age_unit)) +
      scale_pair_color_manual(c(1, 2)) +
      ylab("ratio") +
      facet_wrap(~ risk) +
      match_theme(theme = theme, ...)
    legend <- get_legend(g1)
    top <- bquote("Risk Ratio = " ~ frac(.(risk1), .(risk2)))
    p <- grid_left_to_right(g1, g2, legend, widths = widths) |>
      add_top(top, fontsize = 12, heights = heights)
    p
  }
  invisible(z)
}
