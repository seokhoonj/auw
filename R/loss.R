#' Check covered KCD codes
#'
#' Create a binary matrix indicating which KCD codes in `kcd` are covered
#' by each element of `cvd_kcd`. Each column corresponds to one covered
#' code, and each row corresponds to one element of `kcd`.
#'
#' @param cvd_kcd Character vector of covered KCD codes (patterns).
#' @param kcd Character vector of KCD codes to be checked.
#'
#' @return A binary matrix with `length(kcd)` rows and `length(cvd_kcd)` columns,
#'   where element `[i, j]` is `1` if `kcd[i]` matches `cvd_kcd[j]`, otherwise `0`.
#'
#' @examples
#' head(check_cvd_kcd(c("A00", "A01", "A02", "A03"), kcd_book$kcd), 20)
#'
#' @export
check_cvd_kcd <- function(cvd_kcd, kcd) {
  m <- length(kcd)
  n <- length(cvd_kcd)
  z <- instead::zeros(dim = c(m, n))
  for (i in seq_along(cvd_kcd)) {
    instead::replace_cols_in_mat(z, i, grepl(cvd_kcd[i], kcd, perl = TRUE))
  }
  return(z)
}

#' Check covered surgery levels
#'
#' Create a binary matrix indicating which surgery levels in `level`
#' are covered by each element of `cvd_level`. Each column corresponds to one
#' covered level, and each row corresponds to one element of `level`.
#'
#' @param cvd_level Integer (or character) vector of covered surgery levels.
#' @param level Integer (or character) vector of surgery levels to be checked.
#'
#' @return A binary matrix with `length(level)` rows and `length(cvd_level)` columns,
#'   where element `[i, j]` is `1` if `level[i]` matches `cvd_level[j]`, otherwise `0`.
#'
#' @examples
#' head(check_cvd_level(c(1, 2, 3, 4, 5), c(1, 1, 3, 2, 4, 5, 2, 3, 2, 5)), 20)
#'
#' @export
check_cvd_level <- function(cvd_level, level) {
  m <- length(level)
  n <- length(cvd_level)
  z <- instead::zeros(c(m, n))
  for (i in seq_along(cvd_level)) {
    instead::replace_cols_in_mat(z, i, grepl(cvd_level[i], level, perl = TRUE))
  }
  return(z)
}

#' Overlap claim matrices
#'
#' Combine two claim matrices (`clm` and `clm_1st`) according to a binary
#' indicator for one-time versus continuous payment.
#'
#' - If a claim column corresponds to a one-time payment, values from
#'   `clm_1st` replace those in `clm`.
#' - If a claim column corresponds to a continuous payment, values remain
#'   unchanged in `clm`.
#'
#' @param clm A matrix. Original claim matrix.
#' @param clm_1st A matrix of the same dimension as `clm`. For each column,
#'   only the first claim is `1`, and the rest are `0`.
#' @param col Logical or numeric vector of length equal to `ncol(clm)`.
#'   If `1`, the column is treated as one-time payment; if `0`, as continuous.
#'
#' @return A matrix of the same dimension as `clm`, with one-time payment
#'   columns replaced by `clm_1st`.
#'
#' @export
overlap_matrix <- function(clm, clm_1st, col) {
  if (any(col == 1))
    col <- which(col == 1L)
  clm[, col] <- clm_1st[, col]
  return(clm)
}

#' Difference in periods between start and end dates
#'
#' Compute the number of grouped periods (in months) between two dates.
#'
#' @param start A `Date` vector. Start date(s).
#' @param end   A `Date` vector. End date(s).
#' @param group Integer (>= 1). Group size in months. Default is 1 (monthly).
#'
#' @return An **integer** vector giving the number of periods between `start` and
#'   `end`, grouped by `group` months. `NA` dates yield `NA` results.
#'
#' @examples
#' count_period(as.Date("2020-01-01"), as.Date("2021-01-01"), group = 12)
#' count_period(as.Date(c("2020-01-15","2020-02-01")),
#'              as.Date(c("2020-03-01","2020-02-20")), group = 1)
#'
#' @export
count_period <- function(start, end, group = 1) {
  stopifnot(inherits(start, "Date"), inherits(end, "Date"))
  instead::assert_class(start, "Date")
  instead::assert_class(end, "Date")
  ys <- data.table::year(start)
  ye <- data.table::year(end)
  ms <- data.table::month(start)
  me <- data.table::month(end)
  ceiling(((ye - ys) * 12 + (me - ms) + 1) / group)
}
