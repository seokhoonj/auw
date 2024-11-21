#' Inforce statistic by period
#'
#' Get inforce statistic by period
#'
#' @param df a cohort data frame
#' @param id_var a name of id variable
#' @param group_var names of group variables
#' @param from_var a name of start date variable
#' @param to_var a name of end date variable
#' @param months how many months for a single period?
#' @return a data frame
#'
#' @export
get_inforce_period <- function(df, id_var, group_var, from_var, to_var, months = 1L) {
  id_var    <- jaid::match_cols(df, rlang::as_name(rlang::enquo(id_var)))
  group_var <- jaid::match_cols(df, sapply(rlang::enexpr(group_var), rlang::as_name))
  from_var  <- jaid::match_cols(df, rlang::as_name(rlang::enquo(from_var)))
  to_var    <- jaid::match_cols(df, rlang::as_name(rlang::enquo(to_var)))
  all_vars  <- c(id_var, group_var, from_var, to_var)
  dt <- data.table::copy(df[, .SD, .SDcols = all_vars])
  p <- jaid::mondiff(dt[[from_var]], dt[[to_var]])
  dt[, `:=`(period, p)]
  group_vars <- c("period", group_var)
  inforce <- dt[, .(n = uniqueN(.SD)), keyby = group_vars, .SDcols = id_var][order(-period)]
  inforce[, `:=`(n, cumsum(n)), keyby = group_var]
  inforce <- inforce[order(period)]
  inforce_add <- inforce[, .(period = min(period) - 1, n = max(n)),
                         keyby = group_var][period > 0]
  if (nrow(inforce_add) > 0) {
    inforce_add <- jaid::rep_row(inforce_add, inforce_add$period)
    inforce_add[, `:=`(period, data.table::frank(period, ties.method = "first")),
                keyby = group_var]
    inforce <- rbind(inforce_add, inforce)
    data.table::setorderv(inforce, group_vars)
  }
  if (months > 1) {
    inforce[, `:=`(period, (period - 1)%/%months + 1)]
    return(inforce[, .(n = mean(n)), keyby = group_vars])
  }
  return(inforce[])
}

#' Inforce statistic by year, month, period
#'
#' Get inforce statistic by year, month, period
#'
#' @param df a data.frame
#' @param id_var a name of id variable
#' @param group_var names of group variables
#' @param from_var a name of start date variable
#' @param to_var a name of end date variable
#' @return a data frame
#'
#' @export
get_inforce_period_ym <- function(df, id_var, group_var, from_var, to_var) {
  id_var    <- jaid::match_cols(df, rlang::as_name(rlang::enquo(id_var)))
  group_var <- jaid::match_cols(df, sapply(rlang::enexpr(group_var), rlang::as_name))
  from_var  <- jaid::match_cols(df, rlang::as_name(rlang::enquo(from_var)))
  to_var    <- jaid::match_cols(df, rlang::as_name(rlang::enquo(to_var)))
  all_vars  <- c(id_var, group_var, from_var, to_var)
  group_vars <- c(group_var, from_var, to_var)
  date_vars <- c(from_var, to_var)
  dt <- data.table::copy(df[, .SD, .SDcols = all_vars])
  dt[, (date_vars) := lapply(.SD, bmonth), .SDcols = date_vars]
  dm <- dt[, .(n = uniqueN(.SD)), keyby = group_vars, .SDcols = id_var]
  ym_list <- jaid::seqvec(dm[[from_var]], dm[[to_var]], by = "month")
  times <- sapply(ym_list, length)
  inforce <- jaid::rep_row(dm, times)
  inforce[, `:=`(ym, as.Date(unlist(ym_list), origin = "1970-01-01"))]
  p <- jaid::mondiff(inforce[[from_var]], inforce[["uym"]])
  inforce[, `:=`(period, p)]
  inforce[, `:=`(uym, jaid::add_mon(cym, -period+1))]
  vars <- c(group_var, "uym", "cym", "period")
  return(inforce[, .(n = sum(n)), keyby = vars])
}

