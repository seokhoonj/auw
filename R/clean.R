#' Set age band
#'
#' Set age band
#'
#' @param df a data.frame
#' @param age_var a name of column specifying age column
#' @param interval a numeric specifying an age inteval (default: 5)
#' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param col_nm a string sepcifying age band column name
#' @param labels a string vector specifying age band label
#' @return no return value
#'
#' @examples
#' \dontrun{
#' df <- data.frame(age = sample(0:99, 20, replace = TRUE))
#' set_age_band(df, age)}
#'
#' @export
set_age_band <- function(df, age_var, interval = 5, right = FALSE,
                         col_nm = "age_band", labels) {
  age_var <- rlang::as_name(rlang::enquo(age_var))
  old_class <- class(df)
  jaid::set_dt(df)
  age <- df[[age_var]]
  mn <- floor(min(age)/interval) * interval
  mx <- ceiling(max(age)/interval) * interval
  if (max(age) == mx)
    mx <- ceiling(max(age)/interval + 1) * interval
  age_band <- cut(age, breaks = seq(mn, mx, interval), right = right,
                  dig.lab = 5)
  if (missing(labels)) {
    l <- levels(age_band)
    r <- gregexpr("[0-9]+", l, perl = TRUE)
    m <- regmatches(l, r)
    s <- as.integer(sapply(m, function(x) x[1L]))
    e <- as.integer(sapply(m, function(x) x[2L])) - 1
    labels <- sprintf("%d-%d", s, e)
  }
  levels(age_band) <- labels
  data.table::set(df, j = col_nm, value = ordered(age_band))
  data.table::setcolorder(df, col_nm, after = age_var)
  jaid::set_attr(df, "class", old_class)
}
