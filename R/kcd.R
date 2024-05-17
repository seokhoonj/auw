#' Get KCD codes
#'
#' Search for KCD codes in Korean or English
#'
#' @param x a KCD code string or regular expression.
#' @param lang a string specifying language to explain ("ko", "en")
#' @param type a string sepcifying a type of a return value ("kcd", "ko", "en")
#' @return a string vector
#'
#' @examples
#' # get a kcd code
#' \donttest{get_kcd("M51")
#' get_kcd("M51$|S33$")
#' get_kcd("Diabetes")}
#'
#' # get a related kcd code vector
#' \donttest{kcd <- get_kcd("M51$|S33$", type = "kcd")
#' kcd}
#'
#' @export
get_kcd <- function(x, lang = c("ko", "en"), type = c("kcd", "ko", "en")) {
  if (missing(x))
    stop("Please insert kcd code string or regular expressions.")
  x <- toupper(x)
  lang <- match.arg(lang)
  type <- match.arg(type)
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

#' Set kcd sub columns
#'
#' Set kcd sub columns.
#'
#' @param df a data.frame
#' @param kcd_var a name of a kcd column
#' @param digit a stop digit of a string vector
#' @return no return values.
#'
#' @examples
#' # Set kcd sub columns
#' \dontrun{
#' set_kcd_sub(df, kcd)}
#'
#' @export
set_kcd_sub <- function(df, kcd_var, digit = c(3L, 2L, 1L)) {
  has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  jaid::set_dt(df)
  kcd_var <- jaid::match_cols(df, sapply(rlang::enexpr(kcd_var), rlang::as_name))
  cols <- sprintf("%s%d", kcd_var, digit)
  for (i in seq_along(cols))
    data.table::set(df, j = cols[i], value = substr(df[[kcd_var]], 1L, digit[i]))
  data.table::setcolorder(df, cols, after = kcd_var)
  jaid::set_attr(df, "class", old_class)
}

#' Set kcd name
#'
#' Set kcd name (en, ko).
#'
#' @param df a data.frame
#' @param kcd_var a name of a kcd column
#' @param dots a logical whether a kcd column contains dots
#' @param lang a string specifying language ("ko", "en")
#' @return no return values.
#'
#' @examples
#' # Set kcd sub columns
#' \dontrun{
#' set_kcd_sub(df, kcd)}
#'
#' @export
set_kcd_name <- function(df, kcd_var, dots = TRUE, lang = c("ko", "en")) {
  # has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  # jaid::set_dt(df)
  copybook <- copy(kcd_book)
  if (dots) jaid::rm_punct(copybook, kcd)
  kcd_var <- jaid::match_cols(df, sapply(rlang::enexpr(kcd_var), rlang::as_name))
  data.table::setnames(copybook, "kcd", kcd_var)
  lang <- match.arg(lang)
  new_kcd_var <- paste0(kcd_var, "_", lang)
  en <- kcd <- ko <- NULL
  if (lang == "ko") {
    df[copybook, on = kcd_var, (new_kcd_var) := ko]
  }
  else if (lang == "en") {
    df[copybook, on = kcd_var, (new_kcd_var) := en]
  }
  # jaid::set_attr(df, "class", old_class)
  invisible(df[])
}

#' Get reserved disease list
#'
#' Get reserved disease list.
#'
#' @return reserved disease list
#'
#' @export
get_diz_list <- function() {
  nms <- local(ls(), envir = .AUW_ENV)
  diz <- lapply(nms, function(x) get(x, envir = .AUW_ENV))
  names(diz) <- nms
  return(diz)
}
