#' Get pattern to search strings
#'
#' Get regular expression pattern to search strings
#'
#' @param x a string vector
#' @return a string vector
#'
#' @examples
#' # get regular expression to search string
#' \donttest{get_search_pattern(c("a|b", "a|c", "c|de|f"))}
#'
#' @export
get_search_pattern <- function(x, collapse = "|")
  paste_uni_str(
    paste0(
      split_str(
        paste_str(
          x, collapse = collapse
        ),
        split = collapse
      ), "$"),
    collapse = collapse
  )

get_exclude_pattern <- function(pattern)
  paste0("^((?!", pattern, ").)*$")
