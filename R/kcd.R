#' Get KCD codes (search by code or term)
#'
#' Search the packaged KCD dictionary for matches in **code**, **Korean term**,
#' or **English term**, and print a compact table of matches. Returns a character
#' vector of the requested field.
#'
#' @param x Character scalar. A KCD code string or a regular expression.
#'   Matching is case-insensitive for codes and English; dots in codes can be
#'   ignored (e.g., `"M51"` matches `"M51.0"`).
#' @param lang Output language for the printed table header/terms; one of
#'   `"ko"` or `"en"`. This does **not** affect the return type; see `type`.
#' @param type Field to return as a character vector; one of `"kcd"`, `"ko"`,
#'   or `"en"`.
#'
#' @return A character vector of the requested `type` (invisible). A formatted
#'   table of matches is printed to the console as a side effect.
#'
#' @details
#' The search proceeds in this order:
#' 1) code column (`kcd`), allowing both dotted and undotted matches
#' 2) Korean term column (`ko`)
#' 3) English term column (`en`)
#' The first non-empty match set is printed and returned.
#'
#' @examples
#' \donttest{
#' # Search by exact/regex code
#' get_kcd("M51")
#' get_kcd("M51$|S33$")
#'
#' # Search by English term (case-insensitive)
#' get_kcd("Diabetes", lang = "en", type = "kcd")
#'
#' # Get matched Korean terms as a character vector
#' ko_terms <- get_kcd("M51$|S33$", type = "ko")
#' }
#'
#' @seealso [add_kcd_sub()], [add_kcd_name()]
#'
#' @export
get_kcd <- function(x, lang = c("ko", "en"), type = c("kcd", "ko", "en")) {
  if (missing(x))
    stop("Please insert kcd code string or regular expressions.")
  lang <- match.arg(lang)
  type <- match.arg(type)

  x <- toupper(x)

  if (any(grepl(x, auw::kcd_book$kcd) |
          grepl(x, gsub("\\.", "", auw::kcd_book$kcd, ignore.case = TRUE)))) {
    if (lang[[1L]] == "ko") {
      df <- auw::kcd_book[
        grepl(x, auw::kcd_book$kcd, ignore.case = TRUE) |
          grepl(x, gsub("\\.", "", auw::kcd_book$kcd, ignore.case = TRUE)),
      ]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$ko))
      iter <- nc + nchar(" | ") + ceiling(rc * 1.6)
      line <- draw_line(min(iter, options()$width))
      code <- stringr::str_pad(df$kcd, width = nc, side = "right")
      result <- paste(paste(code, "|", df$ko), collapse = "\n")
    }
    else {
      df <- auw::kcd_book[
        grepl(x, auw::kcd_book$kcd, ignore.case = TRUE) |
          grepl(x, gsub("\\.", "", auw::kcd_book$kcd, ignore.case = TRUE)),
      ]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$en))
      iter <- nc + nchar(" | ") + rc
      line <- draw_line(min(iter, options()$width))
      code <- stringr::str_pad(df$kcd, width = nc, side = "right")
      result <- paste(paste(code, "|", df$en), collapse = "\n")
    }
    cat(line, "\n")
    cat(result, "\n")
    cat(line, "\n")
    invisible(df[[type[[1L]]]])
  }
  else if (any(grepl(x, auw::kcd_book$ko))) {
    if (lang[[1L]] == "ko") {
      df <- auw::kcd_book[grepl(x, auw::kcd_book$ko, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$ko))
      iter <- nc + nchar(" | ") + ceiling(rc * 1.6)
      line <- draw_line(min(iter, options()$width))
      code <- stringr::str_pad(df$kcd, width = nc, side = "right")
      result <- paste(paste(code, "|", df$ko), collapse = "\n")
    }
    else {
      df <- auw::kcd_book[grepl(x, auw::kcd_book$ko, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$en))
      iter <- nc + nchar(" | ") + rc
      line <- draw_line(min(iter, options()$width))
      code <- stringr::str_pad(df$kcd, width = nc, side = "right")
      result <- paste(paste(code, "|", df$en), collapse = "\n")
    }
    cat(line, "\n")
    cat(result, "\n")
    cat(line, "\n")
    invisible(df[[type[[1L]]]])
  }
  else if (any(grepl(x, toupper(auw::kcd_book$en)))) {
    if (lang[[1L]] == "ko") {
      df <- auw::kcd_book[grepl(x, auw::kcd_book$en, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$ko))
      iter <- nc + nchar(" | ") + ceiling(rc * 1.6)
      line <- draw_line(min(iter, options()$width))
      code <- stringr::str_pad(df$kcd, width = nc, side = "right")
      result <- paste(paste(code, "|", df$ko), collapse = "\n")
    }
    else {
      df <- kcd_book[grepl(x, kcd_book$en, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$en))
      iter <- nc + nchar(" | ") + rc
      line <- draw_line(min(iter, options()$width))
      code <- stringr::str_pad(df$kcd, width = nc, side = "right")
      result <- paste(paste(code, "|", df$en), collapse = "\n")
    }
    cat(line, "\n")
    cat(result, "\n")
    cat(line, "\n")
    invisible(df[[type[[1L]]]])
  }
}

#' Add KCD subcode columns
#'
#' Create substring columns from a KCD code column (e.g., first 3/2/1 chars).
#' Useful for grouping by broader sections of the code.
#'
#' @param df A data frame / tibble / data.table.
#' @param kcd_var Column containing KCD codes; **unquoted** name (e.g., `kcd`)
#'   or **character scalar** (e.g., `"kcd"`).
#' @param digit Integer vector of substring widths to extract (default `c(3,2,1)`).
#'
#' @return An object of the **same class as `df`**, with new columns named
#'   `paste0(kcd_var, digit)` inserted after `kcd_var`.
#'
#' @examples
#' \dontrun{
#' df2 <- add_kcd_sub(df, kcd)         # unquoted
#' df3 <- add_kcd_sub(df, "kcd", 3:1)  # character + custom widths
#' }
#'
#' @seealso [add_kcd_name()], [get_kcd()]
#' @export
add_kcd_sub <- function(df, kcd_var, digit = c(3L, 2L, 1L)) {
  instead::assert_class(df, "data.frame")

  env <- ensure_dt_env(df)
  dt <- env$dt

  kcd_var <- instead::capture_names(dt, !!rlang::enquo(kcd_var))
  cols <- sprintf("%s%d", kcd_var, digit)

  for (i in seq_along(cols))
    data.table::set(
      dt, j = cols[i], value = substr(dt[[kcd_var]], 1L, digit[i])
    )
  data.table::setcolorder(dt, cols, after = kcd_var)

  env$restore(dt)
}

#' Add human-readable KCD names (ko/en)
#'
#' Join a codebook to append a human-readable KCD name column (`*_ko` or `*_en`)
#' next to the KCD code column.
#'
#' @param df A data frame / tibble / data.table.
#' @param kcd_var Column with KCD codes; **unquoted** name (e.g., `kcd`)
#'   or **character scalar** (e.g., `"kcd"`).
#' @param dots Logical. If `TRUE` (default), treat dot-variants in codes as
#'   equivalent (e.g., `"M51"` matches `"M51.0"`); implemented by removing
#'   punctuation in the copybook before join.
#' @param lang Language of the name column to add; one of `"ko"` or `"en"`.
#'
#' @return An object of the **same class as `df`**, with an added column named
#'   `paste0(kcd_var, "_", lang)` containing the mapped term.
#'
#' @details
#' A (right) join is performed against the internal KCD copybook. The copybook
#' column is temporarily renamed to match `kcd_var` so that a named-equality
#' join can be performed; the result is restored to the original class of `df`.
#'
#' @examples
#' \dontrun{
#' df2 <- add_kcd_name(df, kcd, lang = "ko")  # unquoted
#' df3 <- add_kcd_name(df, "kcd", dots = TRUE, lang = "en")
#' }
#'
#' @seealso [add_kcd_sub()], [get_kcd()]
#'
#' @export
add_kcd_name <- function(df, kcd_var, dots = TRUE, lang = c("ko", "en")) {
  instead::assert_class(df, "data.frame")
  lang <- match.arg(lang)

  env <- instead::ensure_dt_env(df)
  dt  <- env$dt

  copybook <- copy(kcd_book)
  if (dots) instead::rm_punct(copybook, kcd)

  kcd_var <- instead::capture_names(dt, !!rlang::enquo(kcd_var))

  data.table::setnames(copybook, "kcd", kcd_var)
  new_kcd_var <- paste0(kcd_var, "_", lang)

  en <- kcd <- ko <- NULL
  if (lang == "ko") {
    dt[copybook, on = kcd_var, (new_kcd_var) := ko]
  }
  else if (lang == "en") {
    dt[copybook, on = kcd_var, (new_kcd_var) := en]
  }

  env$restore(dt)
}

#' Get reserved disease list
#'
#' Return all reserved disease objects stored in the internal package environment.
#'
#' @return A named list of reserved-disease objects found in `.AUW_ENV`.
#'
#' @examples
#' \donttest{
#' diz_list <- get_diz_list()
#' names(diz_list)
#' }
#'
#' @seealso [get_kcd()], [add_kcd_name()]
#'
#' @export
get_diz_list <- function() {
  nms <- ls(.AUW_ENV, all.names = TRUE)
  diz <- lapply(nms, function(x) get(x, envir = .AUW_ENV))
  names(diz) <- nms
  diz
}

# Deprecated functions ----------------------------------------------------

#' Deprecated: set_kcd_sub()
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [add_kcd_sub()] instead.
#'
#' @param ... Additional arguments passed to [add_kcd_sub()].
#'
#' @return Same return value as [add_kcd_sub()].
#'
#' @seealso [add_kcd_sub()]
#'
#' @keywords internal
#' @noRd
set_kcd_sub <- function(...) {
  lifecycle::deprecate_warn("0.0.0.9001", "set_kcd_sub()", "add_kcd_sub()")
  add_kcd_sub(...)
}

#' Deprecated: set_kcd_name()
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [add_kcd_name()] instead.
#'
#' @param ... Additional arguments passed to [add_kcd_name()].
#'
#' @return Same return value as [add_kcd_name()].
#'
#' @seealso [add_kcd_name()]
#'
#' @keywords internal
#' @noRd
set_kcd_name <- function(...) {
  lifecycle::deprecate_warn("0.0.0.9001", "set_kcd_name()", "add_kcd_name()")
  add_kcd_name(...)
}
