#' Split date range by specific date
#'
#' Split date range by specific date.
#'
#' @param df a cohort data frame
#' @param from_var a name of start date variable
#' @param to_var a name of end date variable
#' @param udate a underwriting date
#' @param all a boolean specifying only for splited data or
#' @param verbose a boolean giving verbose information
#' @return a data frame
#'
#' @export
split_date <- function(df, from_var, to_var, udate, all = TRUE,
                       verbose = TRUE) {
  from_var <- rlang::as_name(rlang::enquo(from_var))
  to_var <- rlang::as_name(rlang::enquo(to_var))
  old_class <- class(df)
  jaid::set_dt(df)
  for (i in seq_along(udate)) {
    tmp_e <- df[!(df[[from_var]] < udate[i] & df[[to_var]] >= udate[i])]
    tmp_a <- df[ (df[[from_var]] < udate[i] & df[[to_var]] >= udate[i])]
    tmp_b <- data.table::copy(tmp_a)
    data.table::set(tmp_a, j = to_var, value = udate[i] - 1)
    data.table::set(tmp_b, j = from_var, value = udate[i])
    if (all) {
      dt <- data.table::rbindlist(list(tmp_e, tmp_a, tmp_b))
    }
    else {
      dt <- data.table::rbindlist(list(tmp_a, tmp_b))
    }
    if (verbose)
      cat(sprintf("%s is applied\n", as.Date(udate[i])))
  }
  if (verbose)
    cat("Please check stays or claim year, \nyou may have to re-calculate!\n")
  data.table::setorderv(dt, names(dt))
  jaid::set_attr(dt, "class", old_class)
  jaid::set_attr(df, "class", old_class)
  return(dt)
}
