#' Subset cohort data
#'
#' Subset cohort data using underwriting date and kcd code
#'
#' @param df a cohort data frame
#' @param id_var a name of id variable
#' @param kcd_var a name of kcd variable
#' @param from_var a name of start date variable
#' @param to_var a name of end date variable
#' @param udate a date underwritten
#' @param start xx months before udate (negative numeric)
#' @param end xx months after udate (positive numeric)
#' @param ... kcd codes
#' @examples
#' # subset cohort
#' \dontrun{
#' subset_id_with_kcd(cohort, id, kcd, sdate, edate, udate, -60, 36, "M51")}
#'
#' @export
subset_id_with_kcd <- function(df, id_var, kcd_var, from_var, to_var, udate,
                               start, end, ...) {
  jaid::has_ptr(df, error_raise = TRUE)
  id_var   <- rlang::as_name(rlang::enquo(id_var))
  kcd_var  <- rlang::as_name(rlang::enquo(kcd_var))
  from_var <- rlang::as_name(rlang::enquo(from_var))
  to_var   <- rlang::as_name(rlang::enquo(to_var))
  dots <- rlang::list2(...)
  old_class <- class(df)
  dt <- jaid::get_copied_dt(df)
  for (i in seq_along(dots)) {
    fdate <- jaid::add_mon(udate, start)
    tdate <- jaid::add_mon(udate, end)
    key <- jaid::get_pattern("^!", dots[[i]])
    diz <- jaid::del_pattern("^!", dots[[i]])
    if (key == "") {
      dt <- dt[unique(dt[(dt[[to_var]] >= fdate & dt[[from_var]] < tdate) &
                         (grepl(diz, dt[[kcd_var]], perl = TRUE)),
                         .SD, .SDcols = id_var]), on = id_var]
    }
    else {
      dt <- dt[!unique(dt[(dt[[to_var]] >= fdate & dt[[from_var]] < tdate) &
                          (grepl(diz, dt[[kcd_var]], perl = TRUE)),
                          .SD, .SDcols = id_var]), on = id_var]
    }
  }
  jaid::set_attr(dt, "class", old_class)
  return(dt)
}

#' id with kcd terms
#'
#' Whether the ids have KCD codes that exists before or after a specific point
#' in time
#'
#' Subset cohort data using underwriting date and kcd code
#'
#' @param df a cohort data frame
#' @param id_group_var names of id and group variables like list(id, group, age_band).
#' If no group variables, only a name of id variable.
#' @param kcd_var a name of kcd variable
#' @param from_var a name of start date variable
#' @param to_var a name of end date variable
#' @param udate a string date underwritten
#' @param ... the basic form of kcd terms is like "decl = list(start, end, kcd codes)",
#' start means xx months before udate (negative numeric) and end means xx months
#' after udate (positive months) (ex. list(-36, 36, "M51"))
#' @return a data frame
#'
#' @examples
#' # subset cohort
#' \dontrun{
#' id_with_kcd_terms(cohort, id, list(age_band), kcd, sdate, edate, udate,
#'                   list(-60,  0, "I10"),
#'                   list(-60,  0, "I2[0-5]|I6[0-9]|G46"),
#'                   list(  0, 36, "I2[0-5]|I6[0-9]|G46"))}
#'
#' @export
id_with_kcd_terms <- function(df, id_group_var, kcd_var, from_var, to_var,
                              udate, ...) {
  # class: ir.data (incidence rate data)
  jaid::has_ptr(df, error_raise = TRUE)
  id_group_var <- jaid::match_cols(df, sapply(rlang::enexpr(id_group_var),
                                              rlang::as_name))
  id_var    <- id_group_var[1L]
  kcd_var   <- rlang::as_name(rlang::enquo(kcd_var))
  from_var  <- rlang::as_name(rlang::enquo(from_var))
  to_var    <- rlang::as_name(rlang::enquo(to_var))
  kcd_terms <- rlang::list2(...)
  old_class <- class(df)
  jaid::set_dt(df)
  n <- length(kcd_terms)
  id_list <- vector(mode = "list", length = n + 1L)
  id_list[[1L]] <- unique(df[, .SD, .SDcols = id_group_var])
  id_list[2L:length(id_list)] <- lapply(
    seq_along(kcd_terms),
    function(x) {
      start  <- kcd_terms[[x]][[1L]]
      end    <- kcd_terms[[x]][[2L]]
      kcd_cd <- kcd_terms[[x]][[3L]]
      dt <- subset_id_with_kcd(
        df, id_var = !!id_var, kcd_var = !!kcd_var,
        from_var = !!from_var, to_var = !!to_var,
        udate, start, end, !!!kcd_cd)
      col <- sprintf("%s_%s_%s", paste(kcd_cd, collapse = "+"), start, end)
      ds <- dt[, list(dia = 1), keyby = id_group_var]
      data.table::setnames(ds, "dia", col)
    }
  )
  z <- Reduce(function(...) merge(..., by = id_group_var, all = TRUE), id_list)
  jaid::replace_na_with_zero(z)
  if (!is.null(names(kcd_terms))) {
    new_names <- c(id_group_var, names(kcd_terms))
    idx <- which(new_names == "")
    new_names[idx] <- names(z)[idx]
    data.table::setnames(z, new_names)
  }
  cols <- setdiff(names(z), id_var)
  zs <- z[, list(n = .N), by = cols]
  zs[, `:=`((cols), lapply(.SD, factor)), .SDcols = cols]
  setorderv(zs, cols)
  raw <- data.table::copy(zs)
  jaid::set_attr(raw, "class", old_class)
  jaid::set_attr(z, "raw", raw)
  ratio <- n <- nsum <- NULL
  for (i in 1L:(length(cols) - 1L)) {
    grp <- cols[1L:(length(cols) - i)]
    zs[, `:=`(nsum, sum(n)), by = grp]
    zs[, `:=`(ratio, n / nsum)]
    smry <- data.table::copy(zs)
    jaid::set_attr(smry, "class", old_class)
    jaid::set_attr(z, paste0("summary.", i), smry)
  }
  smry <- data.table::copy(attr(z, "summary.1"))
  jaid::set_attr(z, "summary", smry)
  jaid::set_attr(z, "class", c("ir.data", old_class))
  jaid::set_attr(df, "class", old_class)
  return(z)
}
