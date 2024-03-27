#' Get stat from actual loss ratio
#'
#' Get stat from actual loss ratio data (To-be updated)
#'
#' @param df a data.frame contains actual loss ratio
#' @param group_var name of columns
#' @param value_var name of columns specifying loss, rp
#' @param period_var a name of column specifying period
#' @param elapsed_var a name of column specifying elapsed period
#' @return a data.frame
#'
#' @export
get_stat_alr <- function(df, group_var, value_var = c("loss", "rp"),
                         period_var = c("uym"), elapsed_var = c("elpm")) {
  jaid::has_ptr(df, error_raise = TRUE)
  grp_var <- match_cols(df, sapply(rlang::enexpr(group_var), rlang::as_name))
  val_var <- match_cols(df, sapply(rlang::enexpr(value_var), rlang::as_name))
  prd_var <- match_cols(df, sapply(rlang::enexpr(period_var), rlang::as_name))
  elp_var <- match_cols(df, sapply(rlang::enexpr(elapsed_var), rlang::as_name))
  grp_prd_var <- c(grp_var, prd_var)
  grp_elp_var <- c(grp_var, elp_var)
  grp_prd_elp_var <- c(grp_var, prd_var, elp_var)
  old_class <- class(df)
  jaid::set_dt(df)
  closs <- clr <- cmargin <- cprofit <- crp <- loss <- lr <- n_sample <-
    profit <- rp <- NULL
  dn <- jaid::get_stat_by(df, group_var = !!grp_elp_var,
                          value_var = !!prd_var, fun = jaid::unilen)
  data.table::setnames(dn, prd_var, "n_sample")
  dt <- jaid::get_stat_by(df, group_var = !!grp_prd_elp_var,
                          value_var = !!val_var)
  dt[dn, on = grp_elp_var, `:=`(n_sample, n_sample)]
  data.table::setcolorder(dt, "n_sample", before = prd_var)
  jaid::set_stat_by(dt, group_var = !!grp_prd_var,
                    value_var = !!val_var, fun = cumsum)
  dt[, `:=`(margin, rp - loss)]
  dt[, `:=`(cmargin, crp - closs)]
  dt[, `:=`(profit, factor(ifelse(margin >= 0, "pos", "neg"), levels = c("pos", "neg")))]
  dt[, `:=`(cprofit, factor(ifelse(cmargin >= 0, "pos", "neg"), levels = c("pos", "neg")))]
  dt[, `:=`(lr, loss / rp)]
  dt[, `:=`(clr, closs / crp)]

  dc <- data.table::melt(dt, id.vars = grp_prd_elp_var,
                         measure.vars = c("closs", "crp"))
  da <- dt[, .(
    n_sample   = .N,
    lr_mean    = mean(lr),
    lr_se      = mean(lr) / sqrt(.N),
    clr_mean   = mean(clr),
    clr_se     = mean(clr) / sqrt(.N),
    lr_se_lwr  = mean(lr)  - mean(lr)  / sqrt(.N),
    lr_se_upp  = mean(lr)  + mean(lr)  / sqrt(.N),
    clr_se_lwr = mean(clr) - mean(clr) / sqrt(.N),
    clr_se_upp = mean(clr) + mean(clr) / sqrt(.N)
  ), keyby = grp_elp_var]

  dm <- dt[, .(
    n_sample   = .N,
    lr_median  = median(lr),
    lr_se      = median(lr) / sqrt(.N),
    clr_median = median(clr),
    clr_se     = median(clr) / sqrt(.N),
    lr_se_lwr  = median(lr)  - median(lr)  / sqrt(.N),
    lr_se_upp  = median(lr)  + median(lr)  / sqrt(.N),
    clr_se_lwr = median(clr) - median(clr) / sqrt(.N),
    clr_se_upp = median(clr) + median(clr) / sqrt(.N)
  ), keyby = grp_elp_var]

  jaid::set_attr(dc, "class", c("alr.data.longer", old_class))
  jaid::set_attr(dm, "class", c("alr.data.median", old_class))
  jaid::set_attr(da, "class", c("alr.data.mean", old_class))
  jaid::set_attr(dt, "class", c("alr.data", old_class))
  jaid::set_attr(dt, "melt", dc)
  jaid::set_attr(dt, "median", dm)
  jaid::set_attr(dt, "mean", da)
  jaid::set_attr(df, "class", old_class)
  return(dt[])
}

#' Longer data structure
#'
#' Make a data structure longer
#'
#' @param data a data.frame
#' @param ... further arguments passed to or from other methods.
#'
#' @export
longer <- function(data, ...) {
  UseMethod("longer")
}

#' @method longer alr.data
#' @export
longer.alr.data <- function(data, ...) {
  jaid::assert_class(data, "alr.data")
  attr(data, "longer")
}

#' @method mean alr.data
#' @export
mean.alr.data <- function(x, ...) {
  jaid::assert_class(x, "alr.data")
  attr(x, "mean")
}

#' @method median alr.data
#' @export
median.alr.data <- function(x, ...) {
  jaid::assert_class(x, "alr.data")
  attr(x, "median")
}

#' Actual loss ratio by each UY months
#'
#' Draw an actual loss ratio by each UY months.
#'
#' @param x an alr.data object
#' @param group_var a name of the group variable
#' @param period_var a name of the period variable ("uym", "uy")
#' @param elapsed_var a name of the elapsed variable ("elpm", "elp")
#' @param scales Should `scales` be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @param ... ggshort theme arguments
#' @return a ggplot object
#'
#' @export
alr_uym_plot <- function(x, group_var, period_var = "uym", elapsed_var = "elpm",
                         scales = c("fixed", "free_y", "free_x", "free"),
                         theme = c("view", "save", "shiny"), ...) {
  jaid::assert_class(x, "alr.data")
  period  <- rlang::ensym(period_var)
  elapsed <- rlang::ensym(elapsed_var)
  grp_var <- jaid::match_cols(x, sapply(rlang::enexpr(group_var), rlang::as_name))
  prd_var <- jaid::match_cols(x, sapply(rlang::enexpr(period_var), rlang::as_name))
  elp_var <- jaid::match_cols(x, sapply(rlang::enexpr(elapsed_var), rlang::as_name))
  scales  <- match.arg(scales)
  theme   <- match.arg(theme)

  to <- max(x[[elp_var]])
  start <- min(x[[prd_var]])
  if (jaid::has_cols(x, c("uym", "elpm"))) {
    len <- seq(from = 0, to = to, by = 12)
    breaks <- add_mon(start, len)
  }
  else if (jaid::has_cols(x, c("uy", "elp"))) {
    len <- seq(from = 0, to = to, by = 1)
    breaks <- start + len
  } else {
    stop("The combinations (uym + elpm or uy + elp) are not found.", call. = FALSE)
  }
  clr <- NULL
  ggline(data = x, x = !!elapsed, y = clr, group = !!period, color = !!period) +
    scale_color_gradientn(colours = grDevices::rainbow(length(len)),
                          breaks = breaks) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    facet_wrap(grp_var, scales = scales) +
    ggshort_theme(theme = theme, ...)
}

#' @method plot alr.data
#' @export
plot.alr.data <- function(x, group_var, period_var = "uym", elapsed_var = "elpm",
                          scales = c("fixed", "free_y", "free_x", "free"),
                          theme = c("view", "save", "shiny"), ...) {
  jaid::assert_class(x, "alr.data")
  group_var <- jaid::match_cols(x, sapply(rlang::enexpr(group_var), rlang::as_name))
  period_var <- jaid::match_cols(x, sapply(rlang::enexpr(period_var), rlang::as_name))
  elapsed_var <- jaid::match_cols(x, sapply(rlang::enexpr(elapsed_var), rlang::as_name))
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  alr_uym_plot(x = x, group_var = !!group_var, period_var = !!period_var,
               elapsed_var = !!elapsed_var, scales = scales, theme = theme)
}

#' Mean of actual cumulative loss ratio
#'
#' Draw mean of actual cumulative loss ratio
#'
#' @param x an alr.data object
#' @param group_var a name of the group variable
#' @param period_var a name of the period variable ("uym", "uy")
#' @param elapsed_var a name of the elapsed variable ("elpm", "elp")
#' @param color_type a string of color type, base and deep
#' @param scales Should `scales` be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @param ... ggshort theme arguments
#' @return a ggplot object
#'
#' @export
alr_mean_plot <- function(x, group_var, period_var = "uym", elapsed_var = "elpm",
                          color_type = c("base", "deep"),
                          scales = c("fixed", "free_y", "free_x", "free"),
                          theme = c("view", "save", "shiny"), ...) {
  jaid::assert_class(x, "alr.data.mean")
  elapsed <- rlang::ensym(elapsed_var)
  grp_var <- jaid::match_cols(x, sapply(rlang::enexpr(group_var), rlang::as_name))
  prd_var <- jaid::match_cols(x, sapply(rlang::enexpr(period_var), rlang::as_name))
  elp_var <- jaid::match_cols(x, sapply(rlang::enexpr(elapsed_var), rlang::as_name))
  color_type <- match.arg(color_type)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  clr_mean <- clr_se_lwr <- clr_se_upp <- NULL
  if (!has_cols(x, "gender"))
    return(
      ggline(data = x, x = !!elapsed, y = clr_mean, ymin_err = clr_se_lwr,
             ymax_err = clr_se_upp) +
        geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
        facet_wrap(grp_var, scales = scales) +
        ggshort_theme(theme = theme, ...)
    )
  gender <- NULL
  return(
    ggline(data = x, x = !!elapsed, y = clr_mean, ymin_err = clr_se_lwr,
           ymax_err = clr_se_upp, group = gender, color = gender) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      scale_pair_color_manual(x[["gender"]]) +
      facet_wrap(grp_var, scales = scales) +
      ggshort_theme(theme = theme, ...)
  )
}

#' @method plot alr.data.mean
#' @export
plot.alr.data.mean <- function(x, group_var, period_var = "uym",
                               elapsed_var = "elpm",
                               color_type = c("base", "deep"),
                               scales = c("fixed", "free_y", "free_x", "free"),
                               theme = c("view", "save", "shiny"), ...) {
  jaid::assert_class(x, "alr.data.mean")
  group_var <- jaid::match_cols(x, sapply(rlang::enexpr(group_var), rlang::as_name))
  period_var <- jaid::match_cols(x, sapply(rlang::enexpr(period_var), rlang::as_name))
  elapsed_var <- jaid::match_cols(x, sapply(rlang::enexpr(elapsed_var), rlang::as_name))
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  alr_mean_plot(x = x, group_var = !!group_var, period_var = !!period_var,
                elapsed_var = !!elapsed_var, scales = scales, theme = theme)
}


#' Median of actual cumulative loss ratio
#'
#' Draw a median of actual cumulative loss ratio
#'
#' @param x an alr.data object
#' @param group_var a name of the group variable
#' @param period_var a name of the period variable ("uym", "uy")
#' @param elapsed_var a name of the elapsed variable ("elpm", "elp")
#' @param color_type a string of color type, base and deep
#' @param scales Should `scales` be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @param ... ggshort theme arguments
#' @return a ggplot object
#'
#' @export
alr_median_plot <- function(x, group_var, period_var = "uym",
                            elapsed_var = "elpm",
                            color_type = c("base", "deep"),
                            scales = c("fixed", "free_y", "free_x", "free"),
                            theme = c("view", "save", "shiny"), ...) {
  jaid::assert_class(x, "alr.data.median")
  elapsed <- rlang::ensym(elapsed_var)
  grp_var <- jaid::match_cols(x, sapply(rlang::enexpr(group_var), rlang::as_name))
  prd_var <- jaid::match_cols(x, sapply(rlang::enexpr(period_var), rlang::as_name))
  elp_var <- jaid::match_cols(x, sapply(rlang::enexpr(elapsed_var), rlang::as_name))
  color_type <- match.arg(color_type)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  clr_median <- clr_se_lwr <- clr_se_upp <- NULL
  if (!has_cols(x, "gender"))
    return(
      ggline(data = x, x = !!elapsed, y = clr_median, ymin_err = clr_se_lwr,
             ymax_err = clr_se_upp) +
        geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
        facet_wrap(grp_var, scales = scales) +
        ggshort_theme(theme = theme, ...)
    )
  gender <- NULL
  return(
    ggline(data = x, x = !!elapsed, y = clr_median, ymin_err = clr_se_lwr,
           ymax_err = clr_se_upp, group = gender, color = gender) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      scale_pair_color_manual(x[["gender"]]) +
      facet_wrap(grp_var, scales = scales) +
      ggshort_theme(theme = theme, ...)
  )
}

#' @method plot alr.data.median
#' @export
plot.alr.data.median <- function(x, group_var, period_var = "uym",
                                 elapsed_var = "elpm",
                                 color_type = c("base", "deep"),
                                 scales = c("fixed", "free_y", "free_x", "free"),
                                 theme = c("view", "save", "shiny"), ...) {
  jaid::assert_class(x, "alr.data.median")
  group_var <- jaid::match_cols(x, sapply(rlang::enexpr(group_var), rlang::as_name))
  period_var <- jaid::match_cols(x, sapply(rlang::enexpr(period_var), rlang::as_name))
  elapsed_var <- jaid::match_cols(x, sapply(rlang::enexpr(elapsed_var), rlang::as_name))
  color_type <- match.arg(color_type)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  alr_median_plot(x = x, group_var = !!group_var, period_var = !!period_var,
                  elapsed_var = !!elapsed_var, color_type = color_type,
                  scales = scales, theme = theme)
}

#' Compare actual cumulative loss ratio plot
#'
#' Compare actual comulative loss, risk premium and loss ratio among underwriting year months.
#'
#' @param df a data.frame
#' @param elapsed_num a numeric specifying elapsed months
#' @param period_var a name of the period variable ("uym", "uy")
#' @param elapsed_var a name of the elapsed variable ("elpm", "elp")
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @return a gtable object
#'
#' @export
alr_comp_plot <- function(df, elapsed_num, period_var = "uym", elapsed_var = "elpm",
                          theme = c("view", "save", "shiny")) {
  jaid::assert_class(df, "data.frame")
  period <- rlang::ensym(period_var)
  elapsed <- rlang::ensym(elapsed_var)
  theme <- match.arg(theme)
  dt <- get_stat_alr(df = df, period_var = !!period_var, elapsed_var = !!elapsed_var)

  dm <- melt(dt, id.vars = c(period_var, elapsed_var, "cprofit"),
             measure.vars = c("clr"))
  dm <- dm[dm[[elapsed_var]] == elapsed_num,]
  cprofit <- value <- NULL
  g1 <- ggbar(dm, x = !!period, y = value, fill = cprofit) +
    stat_mean_hline(aes(x = !!period, y = value, fill = NULL)) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    scale_pair_fill_manual(dm[["cprofit"]], pair_levels = c("pos", "neg")) +
    coord_flip() +
    ylab("cum loss ratio") +
    facet_wrap("variable") +
    ggshort_theme(theme = theme) +
    theme(panel.border = element_rect(linewidth = 1),
          strip.background = element_rect(linewidth = 1))
  legend <- get_legend(g1)

  dc <- melt(dt, id.vars = c(period_var, elapsed_var, "cprofit"),
             measure.vars = c("crp", "closs", "cmargin"))
  dc <- dc[dc[[elapsed_var]] == elapsed_num,]
  g2 <- ggbar(dc, x = !!period, y = value, fill = cprofit) +
    stat_mean_hline(aes(x = !!period, y = value, fill = NULL)) +
    scale_pair_fill_manual(dm[["cprofit"]], pair_levels = c("pos", "neg")) +
    coord_flip() +
    xlab("") +
    facet_wrap("variable") +
    ggshort_theme(theme = theme, legend.position = "none", y.size = 0)
  grid_left_to_right(g1, g2, legend, widths = c(3.5, 6.5))
}
