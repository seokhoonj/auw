#' Get stat from actual loss ratio
#'
#' Get stat from actual loss ratio data (To-be updated)
#'
#' @param df a data.frame contains actual loss ratio
#' @param group_var name of columns
#' @param value_var name of columns specifying loss, rp
#' @param period_var a name of column specifying period
#' @param elapse_var a name of column specifying elapsed period
#' @return a data.frame
#'
#' @export
get_stat_alr <- function(df, group_var, value_var = c("loss", "rp"),
                         period_var = c("uym"), elapse_var = c("elp")) {
  jaid::has_ptr(df, error_raise = TRUE)
  group_var  <- match_cols(df, sapply(rlang::enexpr(group_var), rlang::as_name))
  value_var  <- match_cols(df, sapply(rlang::enexpr(value_var), rlang::as_name))
  period_var <- match_cols(df, sapply(rlang::enexpr(period_var), rlang::as_name))
  elapse_var <- match_cols(df, sapply(rlang::enexpr(elapse_var), rlang::as_name))
  group_period_var <- c(group_var, period_var)
  group_elapse_var <- c(group_var, elapse_var)
  group_period_elapse_var <- c(group_var, period_var, elapse_var)
  old_class <- class(df)
  jaid::set_dt(df)
  closs <- clr <- cmargin <- crp <- loss <- lr <- n_sample <- rp <- NULL
  dn <- jaid::get_stat_by(df, group_var = !!group_elapse_var,
                          value_var = !!period_var, fun = jaid::unilen)
  data.table::setnames(dn, period_var, "n_sample")
  dt <- jaid::get_stat_by(df, group_var = !!group_period_elapse_var,
                          value_var = !!value_var)
  dt[dn, on = group_elapse_var, `:=`(n_sample, n_sample)]
  data.table::setcolorder(dt, "n_sample", before = period_var)
  jaid::set_stat_by(dt, group_var = !!group_period_var,
                    value_var = !!value_var, fun = cumsum)
  dt[, `:=`(margin, rp - loss)]
  dt[, `:=`(cmargin, crp - closs)]
  dt[, `:=`(lr, loss / rp)]
  dt[, `:=`(clr, closs / crp)]

  dc <- data.table::melt(dt, id.vars = group_period_elapse_var,
                         measure.vars = c("closs", "cmargin", "crp"))
  dm <- dt[, .(
    n_sample   = .N,
    lr_mean    = mean(lr),
    lr_se      = mean(lr) / sqrt(.N),
    clr_mean   = mean(clr),
    clr_se     = mean(clr) / sqrt(.N),
    lr_se_lwr  = mean(lr)  - mean(lr)  / sqrt(.N),
    lr_se_upp  = mean(lr)  + mean(lr)  / sqrt(.N),
    clr_se_lwr = mean(clr) - mean(clr) / sqrt(.N),
    clr_se_upp = mean(clr) + mean(clr) / sqrt(.N)
  ), keyby = group_elapse_var]

  jaid::set_attr(dc, "class", c("cumsum", old_class))
  jaid::set_attr(dm, "class", c("mean", old_class))
  jaid::set_attr(dt, "class", c("alr.data", old_class))
  jaid::set_attr(dt, "cumsum", dc)
  jaid::set_attr(dt, "mean", dm)
  jaid::set_attr(df, "class", old_class)
  return(dt[])
}

#' @method mean alr.data
#' @export
mean.alr.data <- function(x, ...) {
  jaid::assert_class(x, "alr.data")
  attr(x, "mean")
}
