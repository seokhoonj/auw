#' Get pattern to search strings
#'
#' Get regular expression pattern to search strings
#'
#' @param x a string vector
#' @param collapse an optional character string to separate the results. Not
#' [`NA_character_`]. When `collapse` is a string, the result is always a string
#' ([`character`] of length 1). default "|"
#' @return a string vector
#'
#' @examples
#' # get regular expression to search string
#' \donttest{get_search_pattern(c("a|b", "a|c", "c|de|f"))}
#'
#' @export
get_search_pattern <- function(x, collapse = "|") {
  jaid::paste_uni_str(
    paste0(
      strsplit(
        jaid::paste_str(
          x, collapse = collapse
        ),
        split = collapse, fixed = TRUE
      )[[1L]], "$"),
    collapse = collapse
  )
}

get_exclude_pattern <- function(pattern)
  paste0("^((?!", pattern, ").)*$")
