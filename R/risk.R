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
#' @param scales Should `scales` be fixed ("`fixed`", the default), free ("`free`"), or free in one
#' dimension ("`free_x`", "`free_y`")?
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @return a ggplot object
#'
#' @examples
#' # draw a risk rate plot
#' \dontrun{risk_plot(risk_info)}
#'
#' @export
risk_plot <- function(risk_info, x, logscale = FALSE, max_label = TRUE,
                      label_family = "Comic Sans MS", age_interval = 10,
                      nrow = NULL, ncol = NULL, scales = "free_y",
                      theme = c("view", "save", "shiny")) {
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
  }
  else {
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

  fun <- if (logscale) log else force

  g <- ggline(risk_info, x = age, y = fun(rate), color = gender) +
    list(if (max_label) {
      geom_label(data = risk_info_b, aes(label = label),
                 family = label_family, colour = "black", alpha = .3,
                 hjust = -.1, vjust = 1.2)
    }) +
    scale_pair_color_manual(risk_info$gender) +
    scale_x_continuous(n.breaks = floor(jaid::unilen(risk_info$age)/age_interval)) +
    scale_y_continuous(labels = function(x)
      sprintf("%.4f", if (!logscale) x else exp(x))) +
    facet_wrap(~ risk, nrow = nrow, ncol = ncol, scales = scales) +
    ylab(if (!logscale) "rate" else "log(rate)") +
    ggshort_theme(theme = theme)

  jaid::set_attr(risk_info, "class", old_class)
  return(g)
}
