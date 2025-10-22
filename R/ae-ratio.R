#' Summarise Actual-to-Expected (A/E) Statistics
#'
#' @description
#' Compute group-wise statistics for loss and risk premium (`rp`) data, including:
#' - cumulative loss, risk premium, margin, and profit indicators,
#' - sample sizes by period and elapsed period,
#' - actual-to-expected ratio (`aer = loss / rp`) and cumulative A/E ratio (`caer = closs / crp`).
#'
#' This function aggregates experience data by specified grouping,
#' period, and elapsed-period variables, and returns both wide
#' and long-form results.
#'
#' @param df A data.frame containing experience data with loss and risk premium.
#' @param group_var Column(s) used for grouping (e.g., product, gender).
#' @param value_var Character vector specifying the value columns, typically
#'   `c("loss", "rp")` where:
#'   - `loss` = actual claims (observed loss),
#'   - `rp`   = risk premium (expected claims).
#' @param period_var Column(s) defining the exposure period (e.g., underwriting year-month).
#' @param elapsed_var Column(s) defining elapsed periods (e.g., months since issue).
#'
#' @return A data.frame (or tibble/data.table depending on input) with class
#'   `"aer"`, containing the following derived columns:
#'   \describe{
#'     \item{n_sample}{Number of distinct periods observed}
#'     \item{closs, crp}{Cumulative loss and cumulative risk premium}
#'     \item{margin, cmargin}{Period and cumulative margin (`rp - loss`)}
#'     \item{profit, cprofit}{Profit indicator (factor `"pos"`/`"neg"`)}
#'     \item{aer}{Actual-to-Expected ratio (`loss / rp`)}
#'     \item{caer}{Cumulative A/E ratio (`closs / crp`)}
#'   }
#'
#' The returned object also has an attribute `"longer"` containing
#' a melted long-format version (`class = "aer_longer"`).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   product = rep(c("A", "B"), each = 6),
#'   uym = rep(1:3, 4),
#'   elpm = rep(1:2, 6),
#'   loss = runif(12, 80, 120),
#'   rp   = runif(12, 90, 110)  # risk premium (expected)
#' )
#' res <- summarise_aer_stat(
#'   df,
#'   group_var  = product,
#'   value_var  = c("loss", "rp"),
#'   period_var = "uym",
#'   elapsed_var = "elpm"
#' )
#' head(res)
#' attr(res, "longer")
#' }
#'
#' @export
summarise_aer <- function(df,
                          group_var,
                          value_var = c("loss", "rp"),
                          period_var = c("uym"),
                          elapsed_var = c("elpm")) {
  instead::assert_class(df, "data.frame")

  env <- instead::ensure_dt_env(df)
  dt <- env$dt

  grp_var <- instead::capture_names(dt, !!rlang::enquo(group_var))
  val_var <- instead::capture_names(dt, !!rlang::enquo(value_var))
  prd_var <- instead::capture_names(dt, !!rlang::enquo(period_var))
  elp_var <- instead::capture_names(dt, !!rlang::enquo(elapsed_var))

  instead::assert_length(val_var)
  instead::assert_length(prd_var)
  instead::assert_length(elp_var)

  grp_prd_var     <- c(grp_var, prd_var)
  grp_elp_var     <- c(grp_var, elp_var)
  grp_prd_elp_var <- c(grp_var, prd_var, elp_var)

  # closs <- caer <- cmargin <- cprofit <- crp <- loss <- aer <- n_sample <-
  #   profit <- rp <- NULL
  dn <- instead::summarise_group_stats(
    dt, group_var = grp_elp_var, value_var = prd_var, fun = instead::unilen
  )
  data.table::setnames(dn, prd_var, "n_sample")

  ds <- instead::summarise_group_stats(
    dt, group_var = grp_prd_elp_var, value_var = val_var
  )
  ds[dn, on = grp_elp_var, `:=`(n_sample = n_sample)]
  data.table::setcolorder(ds, "n_sample", before = prd_var)

  instead::add_group_stats(
    ds, # data.table
    group_var = grp_prd_var,
    value_var = val_var,
    fun = cumsum,
    prefix = "c"
  )

  data.table::set(ds, j = "margin"  , value = ds$rp - ds$loss)
  data.table::set(ds, j = "cmargin" , value = ds$crp - ds$closs)
  data.table::set(ds, j = "profit"  ,
                  value = factor(ifelse(ds$margin >= 0, "pos", "neg"),
                                 levels = c("pos", "neg")))
  data.table::set(ds, j = "cprofit" ,
                  value = factor(ifelse(ds$cmargin >= 0, "pos", "neg"),
                                 levels = c("pos", "neg")))
  data.table::set(ds, j = "aer"     , value = ds$loss / ds$rp)
  data.table::set(ds, j = "caer"    , value = ds$closs / ds$crp)

  dm <- data.table::melt(
    ds, id.vars = grp_prd_elp_var, measure.vars = c("closs", "crp")
  )
  dm <- instead::prepend_class(dm, "aer_longer")

  data.table::setattr(ds, "group_var"  , grp_var)
  data.table::setattr(ds, "value_var"  , val_var)
  data.table::setattr(ds, "period_var" , prd_var)
  data.table::setattr(ds, "elapsed_var", elp_var)
  data.table::setattr(ds, "longer", dm)

  z <- env$restore(ds)

  instead::prepend_class(z, "aer")
}

#' @method longer aer
#' @export
longer.aer <- function(x, ...) {
  instead::assert_class(x, "aer")
  attr(x, "longer")
}

#' @method mean aer
#' @export
mean.aer <- function(x, ...) {
  instead::assert_class(x, "aer")
  grp_elp_var <- c(attr(x, "group_var"), attr(x, "elapsed_var"))

  caer <- aer <- NULL

  z <- x[, .(
    n_sample   = .N,
    aer_mean    = mean(aer ),
    aer_se      = mean(aer ) / sqrt(.N),
    caer_mean   = mean(caer),
    caer_se     = mean(caer) / sqrt(.N),
    aer_se_lwr  = mean(aer ) - mean(aer ) / sqrt(.N),
    aer_se_upp  = mean(aer ) + mean(aer ) / sqrt(.N),
    caer_se_lwr = mean(caer) - mean(caer) / sqrt(.N),
    caer_se_upp = mean(caer) + mean(caer) / sqrt(.N)
  ), keyby = grp_elp_var]

  instead::prepend_class(z, "aer_mean")
}

#' @method median aer
#' @export
median.aer <- function(x, ...) {
  instead::assert_class(x, "aer")
  grp_elp_var <- c(attr(x, "group_var"), attr(x, "elapsed_var"))

  caer <- aer <- NULL

  z <- x[, .(
    n_sample   = .N,
    aer_median  = median(aer ),
    aer_se      = median(aer ) / sqrt(.N),
    caer_median = median(caer),
    caer_se     = median(caer) / sqrt(.N),
    aer_se_lwr  = median(aer ) - median(aer ) / sqrt(.N),
    aer_se_upp  = median(aer ) + median(aer ) / sqrt(.N),
    caer_se_lwr = median(caer) - median(caer) / sqrt(.N),
    caer_se_upp = median(caer) + median(caer) / sqrt(.N)
  ), keyby = grp_elp_var]

  instead::prepend_class(z, "aer_median")
}

#' Actual A/E ratio by each UY months
#'
#' Draw an actual loss ratio by each UY months.
#'
#' @param x an aer object
#' @param group_var a name of the group variable
#' @param period_var a name of the period variable ("uym", "uy")
#' @param elapsed_var a name of the elapsed variable ("elpm", "elp")
#' @param index_var a name of the index variable ("caer", "closs", "crp")
#' @param scales Should `scales` be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param theme a string specifying a [ggshort::switch_theme()] function ("view", "save", "shiny")
#' @param ... [theme_view()], [theme_save()], [theme_shiny()] arguments
#' @return a ggplot object
#'
#' @export
plot_aer <- function(x, group_var, period_var = "uym",
                     elapsed_var = "elpm", value_var = "caer",
                     scales = c("fixed", "free_y", "free_x", "free"),
                     theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "aer")

  grp_var <- instead::capture_names(x, !!rlang::enquo(group_var))
  prd_var <- instead::capture_names(x, !!rlang::enquo(period_var))
  elp_var <- instead::capture_names(x, !!rlang::enquo(elapsed_var))
  val_var <- instead::capture_names(x, !!rlang::enquo(value_var))

  instead::assert_length(prd_var)
  instead::assert_length(elp_var)

  scales  <- match.arg(scales)
  theme   <- match.arg(theme)

  ggshort::ggline(data = x, x = .data[[elp_var]], y = .data[[val_var]],
                  color = .data[[prd_var]], group = .data[[prd_var]]) +
    ggshort::scale_color_by_month_gradientn() +
    ggshort::geom_hline1() +
    facet_wrap(grp_var, scales = scales) +
    ggshort::switch_theme(theme = theme, ...)
}

#' @method plot aer
#' @export
plot.aer <- function(x, group_var, period_var = "uym",
                          elapsed_var = "elpm", index_var = "caer",
                          scales = c("fixed", "free_y", "free_x", "free"),
                          theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "aer")
  grp_var <- instead::capture_names(x, !!rlang::enquo(group_var))
  prd_var <- instead::capture_names(x, !!rlang::enquo(period_var))
  elp_var <- instead::capture_names(x, !!rlang::enquo(elapsed_var))
  idx_var <- instead::capture_names(x, !!rlang::enquo(index_var))
  instead::assert_length(prd_var)
  instead::assert_length(elp_var)
  instead::assert_length(idx_var)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  plot_aer(x = x, group_var = !!grp_var, period_var = !!prd_var,
               index_var = !!idx_var, elapsed_var = !!elp_var,
               scales = scales, theme = theme)
}

#' Mean of actual cumulative loss ratio
#'
#' Draw mean of actual cumulative loss ratio
#'
#' @param x an aer object
#' @param group_var a name of the group variable
#' @param elapsed_var a name of the elapsed variable ("elpm", "elp")
#' @param color_type a string of color type, base and deep
#' @param scales Should `scales` be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param theme a string specifying a [ggshort::switch_theme()] function ("view", "save", "shiny")
#' @param ... [theme_view()], [theme_save()], [theme_shiny()] arguments
#' @return a ggplot object
#'
#' @export
plot_aer_mean <- function(x, group_var, elapsed_var = "elpm",
                          color_type = c("base", "deep"),
                          scales = c("fixed", "free_y", "free_x", "free"),
                          theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "aer_mean")
  elapsed <- rlang::ensym(elapsed_var)
  grp_var <- instead::capture_names(x, !!rlang::enquo(group_var))
  elp_var <- instead::capture_names(x, !!rlang::enquo(elapsed_var))
  instead::assert_length(elp_var)
  color_type <- match.arg(color_type)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  caer_mean <- caer_se_lwr <- caer_se_upp <- NULL
  if (!has_cols(x, "gender"))
    return(
      ggshort::ggline(data = x,
                      x = !!elapsed, y = caer_mean,
                      ymin_err = caer_se_lwr, ymax_err = caer_se_upp) +
        ggshort::geom_hline1() +
        ggplot2::facet_wrap(grp_var, scales = scales) +
        ggshort::switch_theme(theme = theme, ...)
    )
  ggshort::ggline(data = x,
                  x = !!elapsed, y = .data$caer_mean,
                  ymin_err = .data$caer_se_lwr, ymax_err = .data$caer_se_upp,
                  group = .data$gender, color = .data$gender) +
    ggshort::geom_hline1() +
    ggshort::scale_color_gender() +
    ggplot2::facet_wrap(grp_var, scales = scales) +
    ggshort::switch_theme(theme = theme, ...)
}

#' @method plot aer_mean
#' @export
plot.aer_mean <- function(x, group_var, elapsed_var = "elpm",
                               color_type = c("base", "deep"),
                               scales = c("fixed", "free_y", "free_x", "free"),
                               theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "aer_mean")
  grp_var <- instead::capture_names(x, !!rlang::enquo(group_var))
  elp_var <- instead::capture_names(x, !!rlang::enquo(elapsed_var))
  instead::assert_length(elp_var)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  plot_aer_mean(x = x, group_var = !!grp_var, elapsed_var = !!elp_var,
                scales = scales, theme = theme)
}


#' Median of actual cumulative loss ratio
#'
#' Draw a median of actual cumulative loss ratio
#'
#' @param x an aer object
#' @param group_var a name of the group variable
#' @param elapsed_var a name of the elapsed variable ("elpm", "elp")
#' @param color_type a string of color type, base and deep
#' @param scales Should `scales` be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param theme a string specifying a [ggshort::switch_theme()] function ("view", "save", "shiny")
#' @param ... [theme_view()], [theme_save()], [theme_shiny()] arguments
#' @return a ggplot object
#'
#' @export
plot_aer_median <- function(x, group_var, elapsed_var = "elpm",
                            color_type = c("base", "deep"),
                            scales = c("fixed", "free_y", "free_x", "free"),
                            theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "aer_median")
  grp_var <- instead::capture_names(x, !!rlang::enquo(group_var))
  elp_var <- instead::capture_names(x, !!rlang::enquo(elapsed_var))
  instead::assert_length(elp_var)
  color_type <- match.arg(color_type)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  if (!has_cols(x, "gender"))
    return(
      ggshort::ggline(data = x,
                      x = .data[[elp_var]],
                      y = .data$caer_median,
                      ymin_err = .data$caer_se_lwr,
                      ymax_err = .data$caer_se_upp) +
        ggshort::geom_hline1() +
        ggplot2::facet_wrap(grp_var, scales = scales) +
        ggshort::switch_theme(theme = theme, ...)
    )
  ggshort::ggline(data = x,
                  x = .data[[elp_var]], y = caer_median,
                  ymin_err = caer_se_lwr, ymax_err = caer_se_upp,
                  group = .data$gender, color = .data$gender) +
    ggshort::geom_hline1() +
    ggshort::scale_color_gender() +
    ggplot2::facet_wrap(grp_var, scales = scales) +
    ggshort::switch_theme(theme = theme, ...)
}

#' @method plot aer_median
#' @export
plot.aer_median <- function(x, group_var, elapsed_var = "elpm",
                            color_type = c("base", "deep"),
                            scales = c("fixed", "free_y", "free_x", "free"),
                            theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "aer_median")
  grp_var <- instead::capture_names(x, !!rlang::enquo(group_var))
  elp_var <- instead::capture_names(x, !!rlang::enquo(elapsed_var))
  instead::assert_length(elp_var)
  color_type <- match.arg(color_type)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  plot_aer_median(x = x, group_var = !!grp_var, elapsed_var = !!elp_var,
                  color_type = color_type, scales = scales, theme = theme)
}

#' Compare actual cumulative loss ratio plot
#'
#' Compare actual comulative loss, risk premium and loss ratio among underwriting year months.
#'
#' @param df a data.frame
#' @param elapsed_num a numeric specifying elapsed months
#' @param period_var a name of the period variable ("uym", "uy")
#' @param elapsed_var a name of the elapsed variable ("elpm", "elp")
#' @param theme a string specifying a [ggshort::switch_theme()] function ("view", "save", "shiny")
#' @return a gtable object
#'
#' @export
plot_aer_comp <- function(df, elapsed_num, period_var = "uym", elapsed_var = "elpm",
                          theme = c("view", "save", "shiny")) {
  instead::assert_class(df, "data.frame")
  prd_var <- instead::capture_names(df, !!rlang::enquo(period_var))
  elp_var <- instead::capture_names(df, !!rlang::enquo(elapsed_var))
  instead::assert_length(prd_var)
  instead::assert_length(elp_var)
  theme <- match.arg(theme)
  dt <- summarise_aer_stats(
    df = df, period_var = prd_var, elapsed_var = elp_var
  )
  dm <- melt(dt, id.vars = c(prd_var, elp_var, "cprofit"),
             measure.vars = c("caer"))
  dm <- dm[dm[[elp_var]] == elapsed_num,]
  cprofit <- value <- NULL
  g1 <- ggbar(dm, x = prd_var, y = value, fill = cprofit) +
    stat_mean_hline(aes(y = value), inherit.aes = FALSE) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    scale_fill_pair_manual(pair_levels = c("pos", "neg")) +
    facet_wrap("variable") +
    coord_flip() +
    ylab("Cumulative A/E Ratio") +
    ggshort::switch_theme(theme = theme)
  legend <- get_legend(g1)

  dc <- melt(dt, id.vars = c(prd_var, elp_var, "cprofit"),
             measure.vars = c("crp", "closs", "cmargin"))
  dc <- dc[dc[[elp_var]] == elapsed_num,]
  g2 <- ggbar(dc, x = prd_var, y = value, fill = cprofit) +
    ggshort::stat_mean_hline(aes(y = value), inherit.aes = FALSE) +
    ggshort::scale_fill_pair_manual(pair_levels = c("pos", "neg")) +
    ggshort::scale_y_comma() +
    ggplot2::facet_wrap("variable") +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggshort::switch_theme(theme = theme, legend.position = "none", y.size = 0)
  hstack_plots_with_legend(g1, g2, legend)
}
