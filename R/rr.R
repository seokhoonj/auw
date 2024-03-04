#' id with kcd terms for 'Relative Risk'
#'
#' Whether the ids have KCD codes that exists before or after a specific point
#' in time
#'
#' Subset cohort data using underwriting date and kcd code
#'
#' @param df a cohort data frame
#' @param id_group_var names of id and group variables like .(id, group, age_band)
#' or list(id, age_band). If no group variables, only a name of id variable.
#' @param kcd_var a name of kcd variable
#' @param from_var a name of start date variable
#' @param to_var a name of end date variable
#' @param udate a string date underwritten
#' @param decl1,decl2,decl3,excl,claim the basic form of kcd terms is like
#' "decl1 = list(start, end, kcd codes)", start means xx months before udate
#' (negative numeric) and end means xx months after udate (positive months)
#' (ex. decl1 = list(-36, 36, "M51")). decl means declaration, excl means exclusion,
#' claim means claim.
#' @return a data frame
#'
#' @examples
#' # id with kcd terms for 'Relative Risk'
#' \dontrun{
#' id_with_kcd_terms(cohort, id, list(age_band), kcd, sdate, edate, udate,
#'                   decl1 = list(-60,  0, "I10"),
#'                   decl2 = list(-60,  0, "E78"),
#'                   decl3 = list(-60,  0, "E1[0-4]"),
#'                   excl  = list(-60,  0, "I2[0-5]|I6[0-9]|G46"),
#'                   claim = list(  0, 36, "I2[0-5]|I6[0-9]|G46"))}
#'
#' @export
id_with_rr_kcd_terms <- function(df, id_group_var, kcd_var, from_var, to_var,
                                 udate, decl1 = NULL, decl2 = NULL, decl3 = NULL,
                                 excl = NULL, claim = NULL) {
  old_class <- class(df)
  id_group_var <- rlang::enexpr(id_group_var)
  kcd_var  <- rlang::as_name(rlang::enquo(kcd_var))
  from_var <- rlang::as_name(rlang::enquo(from_var))
  to_var   <- rlang::as_name(rlang::enquo(to_var))
  dots <- rlang::list2(decl1 = decl1, decl2 = decl2, decl3 = decl3,
                       excl = excl, claim = claim)
  dots <- dots[sapply(dots, function(x) !is.null(x))]
  dt <- id_with_kcd_terms(df, id_group_var = !!id_group_var,
                          kcd_var = !!kcd_var, from_var = !!from_var,
                          to_var = !!to_var, udate = udate, !!!dots)
  smry <- summary(dt)
  old_class <- class(smry)
  jaid::set_dt(smry)
  decl_cols <- names(smry)[grepl("^decl", names(smry), perl = TRUE)]
  smry$decl <- jaid::paste_list(smry[, .SD, .SDcols = decl_cols], sep = "+")
  smry$decl <- as.factor(smry$decl)
  rm_cols(smry, !!decl_cols)
  data.table::setcolorder(smry, "decl", before = "excl")
  data.table::setattr(smry, "class", c("ir", old_class))
  decl <- list(decl1[[3L]], decl2[[3L]], decl3[[3L]])
  decl <- decl[sapply(decl, function(x) !is.null(x))]
  data.table::setattr(smry, "decl", paste(decl, collapse = " & "))
  data.table::setattr(smry, "excl", excl[[3L]])
  data.table::setattr(smry, "claim", claim[[3L]])
  data.table::setattr(dt, "summary", smry)
  return(dt)
}

#' @method summary ir.data
#' @export
summary.ir.data <- function(object, ...) {
  jaid::assert_class(object, "ir.data")
  attr(object, "summary")
}

#' Incidence Rate Plot
#'
#' Draw an incidence rate plot.
#'
#' @param x ir object
#' @param color_type a string specifying a pair_color_type ("base", "deep")
#' @param scales Should scales be fixed ("fixed", the default), free ("free"),
#' or free in one dimension ("free_x", "free_y")?
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @return a ggplot object
#'
#' @examples
#' # draw an incidence rate plot
#' \dontrun{
#' irplot(x)}
#'
#' @export
irplot <- function(x, color_type = c("base", "deep"),
                   scales = c("fixed", "free_y", "free_x", "free"),
                   theme = c("view", "save", "shiny")) {
  jaid::assert_class(x, "ir")
  title <- "Incidence Rate"
  decl  <- attr(x, "decl")
  excl  <- attr(x, "excl")
  claim <- attr(x, "claim")
  subtitle <- sprintf("Decl: %s - Excl: %s - Claim: %s", decl, excl, claim)
  dt <- x[x$excl == 0 & x$claim == 1,]
  scales <- match.arg(scales)
  color_type <- match.arg(color_type)
  is_pair <- length(levels(dt$decl)) == 2
  ratio <- NULL
  if (jaid::has_cols(dt, c("gender", "age_band")))
    return(
      ggbar(dt, x = age_band, y = ratio, fill = decl) +
        list(if (is_pair)
          scale_pair_fill_manual(dt$decl, color_type = color_type)
        ) +
        facet_wrap(~ gender) +
        labs(title = title, subtitle = subtitle) +
        ggshort_theme(theme = theme, x.angle = 90)
    )
  if (jaid::has_cols(dt, "gender") & !jaid::has_cols(dt, "age_band"))
    return(
      ggbar(dt, x = gender, y = ratio, fill = decl) +
        list(if (is_pair)
          scale_pair_fill_manual(dt$decl, color_type = color_type)
        ) +
        labs(title = title, subtitle = subtitle) +
        ggshort_theme(theme = theme)
    )
  if (!jaid::has_cols(dt, "gender") & jaid::has_cols(dt, "age_band"))
    return(
      ggbar(dt, x = age_band, y = ratio, fill = decl) +
        list(if (is_pair)
          scale_pair_fill_manual(dt$decl, color_type = color_type)
        ) +
        labs(title = title, subtitle = subtitle) +
        ggshort_theme(theme = theme, x.angle = 90)
    )
  if (!jaid::has_cols(dt, "gender") & !jaid::has_cols(dt, "age_band"))
    return(
      ggbar(dt, x = decl, y = ratio, fill = decl) +
        list(if (is_pair)
          scale_pair_fill_manual(dt$decl, color_type = color_type)
        ) +
        labs(title = title, subtitle = subtitle) +
        ggshort_theme(theme = theme)
    )
}

#' @method plot ir
#' @export
plot.ir <- function(x, color_type = c("base", "deep"),
                    scales = c("fixed", "free_y", "free_x", "free"),
                    theme = c("view", "save", "shiny"), ...) {
  color_type <- match.arg(color_type)
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  irplot(x = x, color_type = color_type, scales = scales, theme = theme)
}

#' Relative Risk
#'
#' Get relative risks from the data.rr object.
#'
#' @param x a rr object
#' @param decl_vs a two elements string vector contains declaration items to compare.
#' @param threshold threshold to reject null hypothesis. (default: 0.975)
#' @return a data frame contains relative risks
#'
#' @export
get_rel_risk <- function(x, decl_vs = c("0", "1"), threshold = .975) {
  jaid::assert_class(x, "ir")
  old_class <- class(x)
  decl_levels <- levels(x$decl)
  if (length(setdiff(decl_vs, levels(x$decl))) > 0)
    stop("Invalid declaration levels")
  dt <- x[x$decl %in% decl_vs & x$excl == 0 & x$claim == 1,]
  if (jaid::unilen(dt$decl) > 2)
    stop("Please select two types of declarations to calculate relative risk")
  group <- setdiff(names(dt), c("decl", "excl", "claim", "n", "nsum", "ratio"))
  if (length(group) == 0)
    group <- "."
  fml <- formula(sprintf("%s ~ decl", paste(group, collapse = " + ")))

  dn <- data.table::dcast(dt, fml, value.var = "n", fun.aggregate = sum)
  jaid::replace_na_with_zero(dn)
  data.table::setnames(dn, c(group, c("n01", "n11")))

  ds <- data.table::dcast(dt, fml, value.var = "nsum", fun.aggregate = sum)
  jaid::replace_na_with_zero(ds)
  data.table::setnames(ds, c(group, c("nsum0", "nsum1")))

  n00 <- n01 <- n10 <- n11 <- nsum0 <- nsum1 <- NULL
  ds[dn, on = group, `:=`(n01 = n01, n11 = n11)]
  ds[, `:=`(n00, nsum0 - n01)]
  ds[, `:=`(n10, nsum1 - n11)]
  jaid::rm_cols(ds, list(nsum0, nsum1))
  data.table::setnames(ds,
    c("n11", "n01", "n10", "n00"),
    c("tp" , "fn" , "fp" , "tn")
  )
  m <- array(t(as.matrix(ds[, c("tp", "fn", "fp", "tn")])),
             dim = c(2L, 2L, nrow(ds)))
  pvalue <- sapply(1:nrow(ds), function(x) stats::fisher.test(m[,, x])$p.value)

  inc0 <- inc1 <- fp <- fn <- or <- rr <- tp <- tn <- NULL
  ds[, `:=`(inc0, fn / (fn + tn))]
  ds[, `:=`(inc1, tp / (tp + fp))]
  ds[, `:=`(rr, (inc1 / inc0))]
  ds[, `:=`(or, (tp / fp) / (fn / tn))]
  ds$pvalue <- pvalue
  ds$reject <- factor(ifelse(pvalue < (1 - threshold), 1, 0), levels = c(0, 1))
  data.table::setattr(ds, "decl" , attr(x, "decl"))
  data.table::setattr(ds, "excl" , attr(x, "excl"))
  data.table::setattr(ds, "claim", attr(x, "claim"))
  data.table::setattr(ds, "class", c("rr", old_class[old_class != "ir"]))
  return(ds)
}

#' Relative Risk Plot
#'
#' Draw a relative risk plot
#'
#' @param x a rr object
#' @param logscale a boolean specifying a log scale
#' @param scales Should `scales` be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param theme a string specifying a ggshort theme function ("view", "save", "shiny")
#' @return a ggplot object
#'
#' @examples
#' # relative risk plot
#' \dontrun{
#' rrplot(x)}
#'
#' @export
rrplot <- function(x, logscale = FALSE,
                   scales = c("fixed", "free_y", "free_x", "free"),
                   theme = c("view", "save", "shiny")) {
  jaid::assert_class(x, "rr")
  title <- "Relative Risk"
  decl  <- attr(x, "decl")
  excl  <- attr(x, "excl")
  claim <- attr(x, "claim")
  subtitle <- sprintf("Decl: %s - Excl: %s - Claim: %s", decl, excl, claim)
  scales <- match.arg(scales)
  x$label <- sprintf("%s\n%.2f\n(%s)", ifelse(!x$reject == 1, "N.E.", "") , x$rr, x$tp)
  if (logscale) {
    x$rr <- log(x$rr)
    title <- paste(title, "(log scale)")
  }
  ymax <- max(x$rr, na.rm = TRUE) * 1.3
  label <- rr <- NULL
  if (jaid::has_cols(x, c("gender", "age_band")))
    return(
      ggbar(data = x, x = age_band, y = rr, ymax = ymax, label = label,
            label_size = 3, label_vjust = -.25) +
        facet_wrap(~ gender, scales = scales) +
        labs(title = title, subtitle = subtitle) +
        ggshort_theme(theme = theme, x.angle = 90, y.size = .1)
    )
  if (jaid::has_cols(x, "gender") & !jaid::has_cols(x, "age_band"))
    return(
      ggbar(data = x, x = gender, y = rr, ymax = ymax, label = label,
            label_size = 3, label_vjust = -.25) +
        labs(title = title, subtitle = subtitle) +
        ggshort_theme(theme = theme, y.size = .1)
    )
  if (!jaid::has_cols(x, "gender") & jaid::has_cols(x, "age_band"))
    return(
      ggbar(data = x, x = age_band, y = rr, ymax = ymax, label = label,
            label_size = 3, label_vjust = -.25) +
        labs(title = title, subtitle = subtitle) +
        ggshort_theme(theme = theme, x.angle = 90, y.size = .1)
    )
  if (!jaid::has_cols(x, "gender") & !jaid::has_cols(x, "age_band"))
    return(
      ggbar(data = x, x = "cohort data", y = rr, ymax = ymax, label = label,
            label_size = 3, label_vjust = -.25) +
        labs(title = title, subtitle = subtitle) +
        ggshort_theme(theme = theme, y.size = .1)
    )
}

#' @method plot rr
#' @export
plot.rr <- function(x, logscale = FALSE,
                    scales = c("fixed", "free_y", "free_x", "free"),
                    theme = c("view", "save", "shiny"), ...) {
  scales <- match.arg(scales)
  theme <- match.arg(theme)
  rrplot(x = x, logscale = logscale, scales = scales, theme = theme)
}
