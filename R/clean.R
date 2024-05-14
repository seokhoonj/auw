#' Set age bands
#'
#' Set age bands
#'
#' @param df a data.frame
#' @param age_var a name of column specifying an age column
#' @param interval a numeric specifying an age interval (default: 5)
#' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param col_nm a string specifying age band column name
#' @param cutoff a logical to use maximum age cutoff
#' @param label_type a string vector specifying label type ("close", "open", "open.start", "open.end")
#' @return no return value
#'
#' @examples
#' \dontrun{
#' df <- data.frame(age = sample(0:99, 20, replace = TRUE))
#' set_ptr(df)
#' set_age_band(df, age)}
#'
#' @export
set_age_band <- function(df, age_var, interval = 5, right = FALSE,
                         col_nm = "age_band", cutoff = FALSE,
                         label_type = c("close", "open.start", "open", "open.end")) {
  jaid::assert_class(df, "data.frame")
  age_var <- rlang::as_name(rlang::enquo(age_var))
  label_type <- match.arg(label_type)
  old_class <- class(df)
  jaid::set_dt(df)
  age <- df[[age_var]]
  mn <- floor(min(age)/interval) * interval
  if (!cutoff) {
    mx <- ceiling(max(age)/interval) * interval
    if (max(age) == mx)
      mx <- ceiling(max(age)/interval + 1) * interval
    breaks <- seq(mn, mx, interval)
  } else {
    mx <- max(age) + 1
    breaks <- seq(mn, mx, interval)
    breaks <- sort(unique(c(breaks, mx)))
    breaks <- breaks[breaks <= mx]
  }
  age_band <- cut(age, breaks = breaks, right = right, dig.lab = 5)

  # labels
  l <- levels(age_band)
  r <- gregexpr("[0-9]+", l, perl = TRUE)
  m <- regmatches(l, r)
  s <- as.integer(sapply(m, function(x) x[1L]))
  e <- as.integer(sapply(m, function(x) x[2L])) - 1
  labels <- sprintf("%d-%d", s, e)

  # label type
  if (label_type == "open") {
    labels[1L] <- jaid::get_pattern("-[0-9]+", labels[1L])
    labels[length(labels)] <- jaid::get_pattern("[0-9]+-", labels[length(labels)])
  } else if (label_type == "open.start") {
    labels[1L] <- jaid::get_pattern("-[0-9]+", labels[1L])
  } else if (label_type == "open.end") {
    labels[length(labels)] <- jaid::get_pattern("[0-9]+-", labels[length(labels)])
  }
  levels(age_band) <- labels
  data.table::set(df, j = col_nm, value = ordered(age_band))
  data.table::setcolorder(df, col_nm, after = age_var)
  jaid::set_attr(df, "class", old_class)
}

#' Set bands
#'
#' Set bands
#'
#' @param df a data.frame
#' @param var a name of column
#' @param breaks either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut.
#' @param interval a numeric specifying interval (default: 5)
#' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param col_nm a string specifying a column name
#' @param cutoff a logical to use maximum age cutoff
#' @param label_type a string vector specifying label type ("close", "open", "open.start", "open.end")
#' @return no return value
#'
#' @examples
#' \dontrun{
#' df <- mtcars
#' set_band(df, age)}
#'
#' @export
set_band <- function(df, var, breaks, interval = 5, right = FALSE,
                     col_nm, cutoff = FALSE,
                     label_type = c("close", "open.start", "open", "open.end")) {
  jaid::assert_class(df, "data.frame")
  var <- rlang::as_name(rlang::enquo(var))
  label_type <- match.arg(label_type)
  old_class <- class(df)
  jaid::set_dt(df)
  col <- df[[var]]
  mn <- floor(min(col)/interval) * interval
  if (missing(breaks)) {
    if (!cutoff) {
      mx <- ceiling(max(col)/interval) * interval
      if (max(col) == mx)
        mx <- ceiling(max(col)/interval + 1) * interval
      breaks <- seq(mn, mx, interval)
    } else {
      mx <- max(col) + 1
      breaks <- seq(mn, mx, interval)
      breaks <- sort(unique(c(breaks, mx)))
      breaks <- breaks[breaks <= mx]
    }
  }
  col_band <- cut(col, breaks = breaks, right = right, dig.lab = 5)

  # labels
  l <- levels(col_band)
  r <- gregexpr("[0-9]+", l, perl = TRUE)
  m <- regmatches(l, r)
  s <- as.integer(sapply(m, function(x) x[1L]))
  e <- as.integer(sapply(m, function(x) x[2L])) - 1
  labels <- sprintf("%d-%d", s, e)

  # label type
  if (label_type == "open") {
    labels[1L] <- jaid::get_pattern("-[0-9]+", labels[1L])
    labels[length(labels)] <- jaid::get_pattern("[0-9]+-", labels[length(labels)])
  } else if (label_type == "open.start") {
    labels[1L] <- jaid::get_pattern("-[0-9]+", labels[1L])
  } else if (label_type == "open.end") {
    labels[length(labels)] <- jaid::get_pattern("[0-9]+-", labels[length(labels)])
  }
  levels(col_band) <- labels
  if (missing(col_nm))
    col_nm <- sprintf("%s_band", var)
  data.table::set(df, j = col_nm, value = ordered(col_band))
  data.table::setcolorder(df, col_nm, after = var)
  jaid::set_attr(df, "class", old_class)
}
