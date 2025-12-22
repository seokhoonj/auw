
# A/E ratio ---------------------------------------------------------------

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
                          period_var  = c("uym"),
                          elapsed_var = c("elpm", "cym"),
                          value_var   = c("loss", "rp")) {
  instead::assert_class(df, "data.frame")

  env <- instead::ensure_dt_env(df)
  dt  <- env$dt

  grp_var <- instead::capture_names(dt, !!rlang::enquo(group_var))
  prd_var <- instead::capture_names(dt, !!rlang::enquo(period_var))
  elp_var <- instead::capture_names(dt, !!rlang::enquo(elapsed_var))
  val_var <- instead::capture_names(dt, !!rlang::enquo(value_var))

  instead::assert_length(prd_var)
  instead::assert_length(elp_var)
  instead::assert_length(val_var)

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
  data.table::setattr(ds, "elapsed_var", .get_elapsed_var(elp_var))
  data.table::setattr(ds, "period_var" , prd_var)
  data.table::setattr(ds, "longer", dm)

  z <- env$restore(ds)

  instead::prepend_class(z, "aer")
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
#'
#' @return a ggplot object
#'
#' @examples
#' \dontrun{
#' sa <- summarise_aer(aer)
#' plot(sa, elapsed_var = "cym" , value_var = "aer" , period_var = "uym")
#' plot(sa, elapsed_var = "cym" , value_var = "caer", period_var = "uym")
#' plot(sa, elapsed_var = "elpm", value_var = "aer" , period_var = "uym")
#' plot(sa, elapsed_var = "elpm", value_var = "caer", period_var = "uym")
#' }
#'
#' @export
plot_aer <- function(x,
                     elapsed_var = "elpm",
                     value_var   = "caer",
                     period_var  = "uym",
                     scales      = c("fixed", "free_y", "free_x", "free"),
                     theme       = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "aer")
  scales  <- match.arg(scales)
  theme   <- match.arg(theme)

  grp_var <- attr(x, "group_var")
  prd_var <- instead::capture_names(x, !!rlang::enquo(period_var))
  elp_var <- instead::capture_names(x, !!rlang::enquo(elapsed_var))
  val_var <- instead::capture_names(x, !!rlang::enquo(value_var))

  instead::assert_length(prd_var)
  instead::assert_length(elp_var)
  instead::assert_length(val_var)

  ggshort::ggline(data = x,
                  x = .data[[elp_var]], y = .data[[val_var]],
                  color = .data[[prd_var]], group = .data[[prd_var]]) +
    ggshort::scale_color_by_month_gradientn() +
    ggshort::geom_hline1() +
    ggplot2::facet_wrap(grp_var, scales = scales) +
    ggshort::switch_theme(theme = theme, ...)
}

#' @method plot aer
#' @export
plot.aer <- function(x,
                     elapsed_var = "elpm",
                     period_var  = "uym",
                     value_var   = "caer",
                     scales      = c("fixed", "free_y", "free_x", "free"),
                     theme       = c("view", "save", "shiny"), ...) {
  scales <- match.arg(scales)
  theme  <- match.arg(theme)

  grp_var <- attr(x, "group_var")
  prd_var <- instead::capture_names(x, !!rlang::enquo(period_var))
  elp_var <- instead::capture_names(x, !!rlang::enquo(elapsed_var))
  val_var <- instead::capture_names(x, !!rlang::enquo(value_var))

  plot_aer(x, elapsed_var, value_var, period_var,
           scales = scales, theme = theme)
}

# a/e ratio longer --------------------------------------------------------

#' @method longer aer
#' @export
longer.aer <- function(x, ...) {
  instead::assert_class(x, "aer")
  attr(x, "longer")
}

# a/e ratio mean ----------------------------------------------------------

#' Compute group-wise mean A/E Ratio / Cumulative A/E Ratio
#'
#' S3 method for `mean()` on `aer` objects. Aggregates by the variables stored
#' in attributes `group_var` and `elapsed_var`, computing the mean and standard
#' error (SE) of both `aer` and `caer`. SE bands are reported as mean ± mean/√n.
#'
#' @param x An object of class `aer`.
#' @param ... Unused; included for S3 compatibility.
#'
#' @details
#' The function expects the input to be a `data.table`-like object with columns
#' `aer` and `caer`. Grouping variables are retrieved from
#' `attr(x, "group_var")`, and the elapsed dimension from
#' `attr(x, "elapsed_var")`. Output columns include:
#' \itemize{
#'   \item `n_sample`: number of observations in the cell
#'   \item `aer_mean`, `caer_mean`: group-wise means
#'   \item `aer_se`, `caer_se`: standard errors `mean / sqrt(n)`
#'   \item `aer_se_lwr`, `aer_se_upp`, `caer_se_lwr`, `caer_se_upp`: SE bands
#' }
#'
#' @return A `data.table` grouped grid with the columns above. The result keeps
#' the attributes `group_var` and `elapsed_var`, and its class is updated to
#' include `aer_mean` (via `instead::update_class()`).
#'
#' @examples
#' \donttest{
#' mt <- mean(aer_obj)
#' head(mt)
#' }
#'
#' @method mean aer
#' @export
mean.aer <- function(x, ...) {
  instead::assert_class(x, "aer")

  grp_var     <- attr(x, "group_var")
  elp_var     <- attr(x, "elapsed_var")
  grp_elp_var <- c(grp_var, elp_var)

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

  data.table::setattr(z, "group_var"  , grp_var)
  data.table::setattr(z, "elapsed_var", elp_var)

  instead::update_class(z, "aer", "aer_mean")
}

#' Plot mean A/E Ratio / Cumulative A/E Ratio over elapsed time
#'
#' Visualise group-wise mean AER or CAER with SE bands across the elapsed
#' dimension. If a `gender` column exists, lines are coloured by gender.
#'
#' @param x An object of class `aer_mean` (typically the result of `mean(aer_obj)`).
#' @param value_var Which metric to plot, one of `c("caer", "aer")`. Default shows both in choices; the selected one is drawn.
#' @param color_type Color palette selector, one of `c("base", "deep")`.
#'   (Reserved for future use; currently not altering scales.)
#' @param scales Facet scaling, one of `"fixed"`, `"free_y"`, `"free_x"`, `"free"`.
#' @param theme One of `"view"`, `"save"`, `"shiny"` passed to `ggshort::switch_theme()`.
#' @param ... Additional arguments forwarded to `ggshort::switch_theme()`.
#'
#' @details
#' Facets are created over `attr(x, "group_var")`. The x-axis is the elapsed
#' variable from `attr(x, "elapsed_var")`. SE bands are drawn using the
#' `*_se_lwr` and `*_se_upp` columns.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \donttest{
#' mt <- mean(aer_obj)
#' plot_aer_mean(mt, value_var = "aer", theme = "view")
#' }
#'
#' @export
plot_aer_mean <- function(x,
                          value_var   = c("caer", "aer"),
                          color_type  = c("base", "deep"),
                          scales      = c("fixed", "free_y", "free_x", "free"),
                          theme       = c("view", "save", "shiny"),
                          ...) {
  instead::assert_class(x, "aer_mean")
  color_type <- match.arg(color_type)
  scales     <- match.arg(scales)
  theme      <- match.arg(theme)
  val_var    <- match.arg(value_var)

  val_var_mean   <- paste0(val_var, "_mean")
  val_var_se_lwr <- paste0(val_var, "_se_lwr")
  val_var_se_upp <- paste0(val_var, "_se_upp")

  grp_var <- .rm_gender_var(attr(x, "group_var"))
  elp_var <- attr(x, "elapsed_var")

  has_gender <- has_cols(x, "gender")

  if (!has_gender) {
    return(
      ggshort::ggline(x,
                      x = .data[[elp_var]],
                      y = .data[[val_var_mean]],
                      ymin_err = .data[[val_var_se_lwr]],
                      ymax_err = .data[[val_var_se_upp]]) +
        ggshort::geom_hline1() +
        ggplot2::facet_wrap(grp_var, scales = scales) +
        ggshort::switch_theme(theme = theme, ...)
    )
  }

  ggshort::ggline(x,
                  x = .data[[elp_var]],
                  y = .data[[val_var_mean]],
                  ymin_err = .data[[val_var_se_lwr]],
                  ymax_err = .data[[val_var_se_upp]],
                  group = .data$gender,
                  color = .data$gender) +
    ggshort::geom_hline1() +
    ggshort::scale_color_gender() +
    ggplot2::facet_wrap(grp_var, scales = scales) +
    ggshort::switch_theme(theme = theme, ...)
}

#' Plot method for aer_mean objects
#'
#' S3 method for [plot()] that delegates to [plot_aer_mean()] for objects of
#' class `aer_mean`.
#'
#' @inheritParams plot_aer_mean
#' @param ... Additional arguments forwarded to [plot_aer_mean()].
#'
#' @return A `ggplot` object created by [plot_aer_mean()].
#'
#' @seealso [plot_aer_mean()], [mean.aer()]
#'
#' @examples
#' \donttest{
#' mt <- mean(aer_obj)
#' plot(mt)  # equivalent to plot_aer_mean(mt)
#' }
#'
#' @method plot aer_mean
#' @export
plot.aer_mean <- function(x,
                          value_var   = c("caer", "aer"),
                          color_type  = c("base", "deep"),
                          scales      = c("fixed", "free_y", "free_x", "free"),
                          theme       = c("view", "save", "shiny"),
                          ...) {
  val_var    <- match.arg(value_var)
  color_type <- match.arg(color_type)
  scales     <- match.arg(scales)
  theme      <- match.arg(theme)

  plot_aer_mean(x, value_var = val_var, scales = scales, theme = theme, ...)
}

# a/e ratio median --------------------------------------------------------

#' Compute group-wise median A/E Ratio / Cumulative A/E Ratio
#'
#' S3 method for `median()` on `aer` objects. Aggregates by the variables stored
#' in attributes `group_var` and `elapsed_var`, computing the median and standard
#' error (SE) of both `aer` and `caer`. SE bands are reported as median ± median/√n.
#'
#' @param x An object of class `aer`.
#' @param ... Unused; included for S3 compatibility.
#'
#' @details
#' The function expects the input to be a `data.table`-like object with columns
#' `aer` and `caer`. Grouping variables are retrieved from
#' `attr(x, "group_var")`, and the elapsed dimension from
#' `attr(x, "elapsed_var")`. Output columns include:
#' \itemize{
#'   \item `n_sample`: number of observations in the cell
#'   \item `aer_median`, `caer_median`: group-wise medians
#'   \item `aer_se`, `caer_se`: standard errors `median / sqrt(n)`
#'   \item `aer_se_lwr`, `aer_se_upp`, `caer_se_lwr`, `caer_se_upp`: SE bands
#' }
#'
#' @return A `data.table` grouped grid with the columns above. The result keeps
#' the attributes `group_var` and `elapsed_var`, and its class is updated to
#' include `aer_median` (via `instead::update_class()`).
#'
#' @examples
#' \donttest{
#' mt <- median(aer_obj)
#' head(mt)
#' }
#'
#' @method median aer
#' @export
median.aer <- function(x, ...) {
  instead::assert_class(x, "aer")

  grp_var     <- attr(x, "group_var")
  elp_var     <- attr(x, "elapsed_var")
  grp_elp_var <- c(grp_var, elp_var)

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

  data.table::setattr(z, "group_var"  , grp_var)
  data.table::setattr(z, "elapsed_var", elp_var)

  instead::update_class(z, "aer", "aer_median")
}

#' Plot median A/E Ratio / Cumulative A/E Ratio over elapsed time
#'
#' Visualise group-wise median AER or CAER with SE bands across the elapsed
#' dimension. If a `gender` column exists, lines are coloured by gender.
#'
#' @param x An object of class `aer_median` (typically the result of `median(aer_obj)`).
#' @param value_var Which metric to plot, one of `c("caer", "aer")`. Default shows both in choices; the selected one is drawn.
#' @param color_type Color palette selector, one of `c("base", "deep")`.
#'   (Reserved for future use; currently not altering scales.)
#' @param scales Facet scaling, one of `"fixed"`, `"free_y"`, `"free_x"`, `"free"`.
#' @param theme One of `"view"`, `"save"`, `"shiny"` passed to `ggshort::switch_theme()`.
#' @param ... Additional arguments forwarded to `ggshort::switch_theme()`.
#'
#' @details
#' Facets are created over `attr(x, "group_var")`. The x-axis is the elapsed
#' variable from `attr(x, "elapsed_var")`. SE bands are drawn using the
#' `*_se_lwr` and `*_se_upp` columns.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \donttest{
#' mt <- median(aer_obj)
#' plot_aer_median(mt, value_var = "aer", theme = "view")
#' }
#'
#' @export
plot_aer_median <- function(x,
                            value_var   = c("caer", "aer"),
                            color_type  = c("base", "deep"),
                            scales      = c("fixed", "free_y", "free_x", "free"),
                            theme       = c("view", "save", "shiny"),
                            ...) {
  instead::assert_class(x, "aer_median")
  color_type <- match.arg(color_type)
  scales     <- match.arg(scales)
  theme      <- match.arg(theme)
  val_var    <- match.arg(value_var)

  val_var_median <- paste0(val_var, "_median")
  val_var_se_lwr <- paste0(val_var, "_se_lwr")
  val_var_se_upp <- paste0(val_var, "_se_upp")

  grp_var <- .rm_gender_var(attr(x, "group_var"))
  elp_var <- attr(x, "elapsed_var")

  has_gender <- has_cols(x, "gender")

  if (!has_gender) {
    return(
      ggshort::ggline(x,
                      x = .data[[elp_var]],
                      y = .data[[val_var_median]],
                      ymin_err = .data[[val_var_se_lwr]],
                      ymax_err = .data[[val_var_se_upp]]) +
        ggshort::geom_hline1() +
        ggplot2::facet_wrap(grp_var, scales = scales) +
        ggshort::switch_theme(theme = theme, ...)
    )
  }

  ggshort::ggline(x,
                  x = .data[[elp_var]],
                  y = .data[[val_var_median]],
                  ymin_err = .data[[val_var_se_lwr]],
                  ymax_err = .data[[val_var_se_upp]],
                  group = .data$gender,
                  color = .data$gender) +
    ggshort::geom_hline1() +
    ggshort::scale_color_gender() +
    ggplot2::facet_wrap(grp_var, scales = scales) +
    ggshort::switch_theme(theme = theme, ...)
}

#' Plot method for aer_median objects
#'
#' S3 method for [plot()] that delegates to [plot_aer_median()] for objects of
#' class `aer_median`.
#'
#' @inheritParams plot_aer_median
#' @param ... Additional arguments forwarded to [plot_aer_median()].
#'
#' @return A `ggplot` object created by [plot_aer_median()].
#'
#' @seealso [plot_aer_median()], [median.aer()]
#'
#' @examples
#' \donttest{
#' mt <- median(aer_obj)
#' plot(mt)  # equivalent to plot_aer_median(mt)
#' }
#'
#' @method plot aer_median
#' @export
plot.aer_median <- function(x,
                            value_var   = c("caer", "aer"),
                            color_type  = c("base", "deep"),
                            scales      = c("fixed", "free_y", "free_x", "free"),
                            theme       = c("view", "save", "shiny"),
                            ...) {
  val_var    <- match.arg(value_var)
  color_type <- match.arg(color_type)
  scales     <- match.arg(scales)
  theme      <- match.arg(theme)

  plot_aer_median(x, value_var = val_var, scales = scales, theme = theme, ...)
}


# a/e ratio comparison ----------------------------------------------------

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
plot_aer_comp <- function(df,
                          elapsed_num = 13,
                          period_var  = "uym",
                          elapsed_var = "elpm",
                          theme       = c("view", "save", "shiny"),
                          ...) {
  instead::assert_class(df, "data.frame")
  theme <- match.arg(theme)

  prd_var <- instead::capture_names(df, !!rlang::enquo(period_var))
  elp_var <- instead::capture_names(df, !!rlang::enquo(elapsed_var))

  instead::assert_length(prd_var)
  instead::assert_length(elp_var)

  dt <- summarise_aer(
    df = df, elapsed_var = elp_var, period_var = prd_var
  )
  dm <- data.table::melt(dt,
                         id.vars      = c(prd_var, elp_var, "cprofit"),
                         measure.vars = c("caer"))
  dm <- dm[dm[[elp_var]] == elapsed_num,]

  g1 <- ggshort::ggbar(dm, x = prd_var, y = .data$value, fill = .data$cprofit) +
    ggshort::stat_mean_hline(aes(y = value), inherit.aes = FALSE) +
    ggshort::geom_hline1() +
    ggshort::scale_fill_pair_manual(pair_levels = c("pos", "neg")) +
    ggplot2::facet_wrap("variable") +
    ggplot2::coord_flip() +
    ggplot2::ylab("Cumulative A/E Ratio") +
    ggshort::switch_theme(theme = theme, ...)

  legend <- ggshort::get_legend(g1)

  dc <- data.table::melt(dt,
                         id.vars      = c(prd_var, elp_var, "cprofit"),
                         measure.vars = c("crp", "closs", "cmargin"))
  dc <- dc[dc[[elp_var]] == elapsed_num,]

  g2 <- ggshort::ggbar(dc, x = prd_var, y = .data$value, fill = .data$cprofit) +
    ggshort::stat_mean_hline(aes(y = .data$value), inherit.aes = FALSE) +
    ggshort::scale_fill_pair_manual(pair_levels = c("pos", "neg")) +
    ggshort::scale_y_comma() +
    ggplot2::facet_wrap("variable") +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggshort::switch_theme(theme = theme, legend.position = "none", y.size = 0,
                          ...)

  ggshort::hstack_plots_with_legend(g1, g2, legend)
}


# Internal helper functions -----------------------------------------------

.rm_gender_var <- function(x) {
  x[!grepl("gender", x)]
}

.get_elapsed_var <- function(x) {
  x[grepl("elp", x)]
}

