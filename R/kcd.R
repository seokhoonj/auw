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
get_kcd <- function (x, lang = c("ko", "en"), type = c("kcd", "ko", "en")) {
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
      code <- stringi::stri_pad_right(df$kcd, width = nc)
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
      code <- stringi::stri_pad_right(df$kcd, width = nc)
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
      code <- stringi::stri_pad_right(df$kcd, width = nc)
      result <- paste(paste(code, "|", df$ko), collapse = "\n")
    }
    else {
      df <- auw::kcd_book[grepl(x, auw::kcd_book$ko, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$en))
      iter <- nc + nchar(" | ") + rc
      line <- draw_line(min(iter, options()$width))
      code <- stringi::stri_pad_right(df$kcd, width = nc)
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
      code <- stringi::stri_pad_right(df$kcd, width = nc)
      result <- paste(paste(code, "|", df$ko), collapse = "\n")
    }
    else {
      df <- kcd_book[grepl(x, kcd_book$en, ignore.case = TRUE),]
      nc <- max(nchar(df$kcd))
      rc <- max(nchar(df$en))
      iter <- nc + nchar(" | ") + rc
      line <- draw_line(min(iter, options()$width))
      code <- stringi::stri_pad_right(df$kcd, width = nc)
      result <- paste(paste(code, "|", df$en), collapse = "\n")
    }
    cat(line, "\n")
    cat(result, "\n")
    cat(line, "\n")
    invisible(df[[type[[1L]]]])
  }
}

draw_line <- function(width, mark = "=") {
  if (missing(width))
    width <- options()$width
  paste(rep(mark, times = width), collapse = "")
}
