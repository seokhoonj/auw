#' Create age bands
#'
#' A convenience wrapper around [add_band()] to generate categorical
#' **age bands** from a numeric age column. The function creates an
#' ordered factor column representing grouped age intervals.
#'
#' @param df A data.frame containing an age column.
#' @param age_var Unquoted column name specifying the age variable.
#' @param interval Numeric; the width of each age interval. Default: `5`.
#' @param right Logical; whether intervals should be closed on the right
#'   (and open on the left) or vice versa. Default: `FALSE`.
#' @param col_nm String; name of the new age band column. Default `"age_band"`.
#' @param cutoff Logical; if `TRUE`, ensures the maximum observed age
#'   is included as an upper cutoff. Default: `FALSE`.
#' @param label_style One of `"close"`, `"open"`, `"open.start"`, `"open.end"`;
#'   controls how labels are displayed at boundaries:
#'   - `"close"`: closed intervals like `"0-4"`, `"5-9"`.
#'   - `"open"`: first/last open (e.g. "`-4`", "`95-`").
#'   - `"open.start"`: only first open (e.g. "`-4`", "`5-9"`).
#'   - `"open.end"`: only last open (e.g. "`95-`").
#'
#' @return The input `df`, augmented with a new ordered factor column
#'   (by default `"age_band"`).
#'
#' @examples
#' \donttest{
#' df <- data.frame(age = sample(0:99, 20, replace = TRUE))
#'
#' # Default 5-year bands
#' add_age_band(df, age)
#'
#' # Custom 10-year bands with open-ended labels
#' add_age_band(df, age, interval = 10, label_style = "open.end")
#' }
#'
#' @export
add_age_band <- function(df, age_var, interval = 5, right = FALSE,
                         col_nm = "age_band", cutoff = FALSE,
                         label_style = c("close", "open.start", "open", "open.end")) {
  instead::assert_class(df, "data.frame")
  label_style <- match.arg(label_style)
  age_var <- instead::capture_names(df, !!rlang::enquo(age_var))
  instead::add_band(df = df, num_var = age_var, interval = interval, right = right,
                 col_nm = col_nm, cutoff = cutoff, label_style = label_style)
}

melt_kcd <- function(df, kcd_cols) {
  instead::assert_class(df, "data.frame")

  env <- ensure_dt_env(df)
  dt  <- env$dt

  kcd_cols <- capture_names(dt, !!rlang::enquo(kcd_cols))

  kcd_cols_all <- instead::regex_cols(dt, "^kcd[0-9]?$")
  if (length(kcd_cols_all) == 0L)
    stop("No kcd columns found. Expected columns like 'kcd', 'kcd1', 'kcd2'.",
         call. = FALSE)

  if (!identical(sort(kcd_cols), sort(kcd_cols_all))) {
    missing <- setdiff(kcd_cols_all, kcd_cols)
    extra   <- setdiff(kcd_cols, kcd_cols_all)
    msg <- "Select all kcd columns"
    if (length(missing))
      msg <- paste0(msg, "; missing: ", paste(missing, collapse = ", "))
    if (length(extra))
      msg <- paste0(msg,  "; not-kcd: ", paste(extra,   collapse = ", "))
    stop(msg, call. = FALSE)
  }

  id_cols <- instead::anti_cols(dt, kcd_cols)

  dm <- data.table::melt(
    data = dt,
    id.vars = id_cols,
    measure.vars = kcd_cols,
    variable.name = "ord_kcd",
    value.name = "kcd"
  )

  ord_raw <- sub("^kcd", "", dm$ord_kcd)
  ord_int <- ifelse(nzchar(ord_raw), as.integer(ord_raw), 0L)
  data.table::set(dm, j = "ord_kcd", value = ord_int)

  env$restore(dm)
}

# Deprecated functions ----------------------------------------------------

#' Deprecated: set_age_band()
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [add_age_band()] instead.
#'
#' @param ... Additional arguments passed to [add_age_band()].
#'
#' @return Same return value as [add_age_band()].
#'
#' @seealso [add_age_band()]
#'
#' @export
set_age_band <- function(...) {
  lifecycle::deprecate_warn("0.0.0.9001", "set_age_band()", "add_age_band()")
  add_age_band(...)
}
