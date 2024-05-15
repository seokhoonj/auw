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

limit_stay <- function(df, id_var, merge_var, from_var, to_var, deduction,
                       limit, waiting) {
  id_var <- match_cols(df, vapply(substitute(id_var), deparse,
                                  "character"))
  merge_var <- match_cols(df, vapply(substitute(merge_var),
                                     deparse, "character"))
  from_var <- match_cols(df, vapply(substitute(from_var), deparse,
                                    "character"))
  to_var <- match_cols(df, vapply(substitute(to_var), deparse,
                                  "character"))
  trvs <- traverse(df[[from_var]], df[[to_var]])
  diff <- c(diff(trvs), 1)
  id_trv <- reprow(df[, ..id_var], each = 2L)
  pt <- sort_group_by(id_trv)
  pt_stt <- nolast(pt + 1)
  pt_end <- pt[2:(length(pt) - 1)]
  diff[pt_end] <- 1
  adjs <- rep(c(1, -1), times = length(diff)/2)
  diff <- diff + adjs
  bins <- rep(c(1, 0), times = length(diff)/2)
  stay <- rep(bins, diff)
  dm <- df[, .(from = min(get(from_var)), to = max(get(to_var))),
           id_var]
  set(dm, j = "len", value = as.numeric(dm$to - dm$from + 1))
  stay_mod <- .Call(vuw_limit_stay_in_the_interval, stay, dm$len,
                    limit, waiting)
  from <- num2date(expand_date(dm$from, dm$to))
  dm_id <- reprow(dm[, ..id_var], times = dm$len)
  z <- data.table(dm_id, from = from, stay = stay, stay_mod = stay_mod)
  z <- z[!(stay == 0 & stay_mod == 0)]
  set(z, j = "period", value = bmonth(z$from))
  if (!missing(deduction)) {
    z[, `:=`(rank, rank(from, ties.method = "first")), id_var]
    z[rank <= deduction, `:=`(stay_mod, 0)]
    rm_cols(z, rank)
  }
  id_vars <- c(id_var, "period")
  z <- z[, .(from = min(from), stay = sum(stay), stay_mod = sum(stay_mod)),
         id_vars]
  set(z, j = "to", value = num2date(ifelse(z$stay_mod > 0,
                                           z$from + z$stay_mod - 1L, z$from)))
  setcolafter(z, to, from)
  setnames(z, c("from", "to"), c(from_var, to_var))
  m <- monthly_merge_var_(df, id_var, merge_var, from_var,
                          to_var)
  if (nrow(z) != nrow(m))
    stop("invalid nrows")
  on_var <- c(id_var, "period")
  z <- z[m, on = on_var]
  setcolafter_(z, merge_var, id_var[length(id_var)])
  return(z)
}
