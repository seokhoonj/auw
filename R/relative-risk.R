#' Subset a cohort **by IDs** flagged for KCD within an underwriting window
#'
#' Select **all rows for IDs** who had at least one record with specified
#' KCD code(s) during a window around an underwriting date. The window is
#' defined in **months** relative to `uw_date` (negative = before, positive = after).
#' Matching is performed on the KCD column, but the **result includes every row
#' of those IDs** (i.e., not only the matching KCD rows).
#'
#' @description
#' Given a cohort data frame with ID, KCD, and service date range columns,
#' this function:
#' 1. Computes a date window `[uw_date + start months, uw_date + end months]`
#'    for each record. These boundary dates are stored internally as:
#'    - `fdate`: the window **start date** (underwriting date + start months)
#'    - `tdate`: the window **end date** (underwriting date + end months).
#' 2. Finds IDs whose any row has `to_var >= fdate` and `from_var < tdate`
#'    **and** whose `kcd_var` matches one or more KCD patterns.
#' 3. Returns the **entire** cohort subset for those IDs.
#'
#' @section Selection semantics:
#' - Date overlap uses half-open interval: `[from, to]` via
#'   `to_var >= fdate & from_var < tdate`.
#' - KCD matching uses `grepl(..., perl = TRUE)`; supply regular expressions if needed.
#' - You may prefix a pattern with `"!"` to **exclude** IDs matching that pattern
#'   (applied after any prior inclusions). You can pass multiple patterns via `...`.
#'
#' @param cohort A cohort data frame at **episode level** (multiple rows per ID).
#' @param id_var Column used as the ID; unquoted column name (e.g., `id`).
#' @param kcd_var Column containing KCD codes; unquoted column name (e.g., `kcd`).
#' @param from_var Column with row start dates; unquoted (e.g., `sdate`). Must be `Date`.
#' @param to_var Column with row end dates; unquoted (e.g., `edate`). Must be `Date`.
#' @param uw_date Underwriting reference date of type `Date`.
#' @param start Months **before** `uw_date` to start the window (negative numeric).
#' @param end Months **after** `uw_date` to end the window (positive numeric).
#' @param ... One or more KCD patterns as character scalars. Each element can be
#'   a plain string or a PCRE (perl) regex. Prefix with `"!"` to exclude IDs
#'   matching that pattern (e.g., `"!M51|S33"`).
#'
#' @return A cohort subset of the **same class** as `cohort` (data.frame / tibble / data.table),
#'   containing **all rows** for IDs that satisfied the KCD-in-window condition (after
#'   applying any exclusions).
#'
#' @details
#' Internally converts to data.table for speed via `instead::ensure_dt_env()`,
#' computes the inclusion/exclusion ID set using `grepl(perl = TRUE)`,
#' and restores the original input class on return.
#'
#' @examples
#' \dontrun{
#' # Include IDs with any M51 within [-60, +36] months of uw_date, then keep ALL their rows
#' subset_cohort_by_id_with_kcd(
#'   cohort, id, kcd, sdate, edate,
#'   uw_date = as.Date("2018-07-01"),
#'   start = -60, end = 36,
#'   "M51"
#' )
#'
#' # Include M79 or S33, then exclude any ^C codes by regex
#' subset_cohort_by_id_with_kcd(
#'   cohort, id, kcd, sdate, edate,
#'   uw_date = as.Date("2018-07-01"),
#'   start = -24, end = 12,
#'   "M79|S33", "!^C"
#' )
#' }
#'
#' @seealso
#' [`summarise_id_with_kcd()`] to get the **distinct ID list** without expanding to full rows.
#'
#' @export
subset_cohort_by_id_with_kcd <- function(cohort, id_var, kcd_var,
                                         from_var, to_var,
                                         uw_date, start, end, ...) {
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt  <- env$dt

  id_var   <- instead::capture_names(dt, !!rlang::enquo(id_var))
  kcd_var  <- instead::capture_names(dt, !!rlang::enquo(kcd_var))
  from_var <- instead::capture_names(dt, !!rlang::enquo(from_var))
  to_var   <- instead::capture_names(dt, !!rlang::enquo(to_var))

  uw_date  <- .resolve_uw_date(dt, !!rlang::enquo(uw_date))

  fdate <- instead::add_mon(uw_date, start)
  tdate <- instead::add_mon(uw_date, end)

  dots <- rlang::list2(...)
  for (i in seq_along(dots)) {
    key <- instead::get_pattern("^!", dots[[i]])
    diz <- instead::del_pattern("^!", dots[[i]])
    if (key == "") {
      dt <- dt[unique(dt[(dt[[to_var]] >= fdate & dt[[from_var]] < tdate) &
                           (grepl(diz, dt[[kcd_var]], perl = TRUE)),
                         .SD, .SDcols = id_var]), on = id_var]
    }
    else {
      dt <- dt[!unique(dt[(dt[[to_var]] >= fdate & dt[[from_var]] < tdate) &
                            (grepl(diz, dt[[kcd_var]], perl = TRUE)),
                          .SD, .SDcols = id_var]), on = id_var]
    }
  }

  env$restore(dt)
}

#' Summarise IDs with KCD terms
#'
#' Starting from an *episode-level* cohort (multiple rows per ID with
#' `[from,to]` and `kcd`), produce an **ID-level summary table** with
#' 0/1 flag columns for each specified KCD window relative to `uw_date`.
#' Internally converts to data.table (via [ensure_dt_env()]) for
#' efficiency, then restores the result and attached summaries to the
#' original class of `cohort` (data.frame / tibble / data.table).
#'
#' @param cohort A cohort data frame at **episode level** (multiple rows per ID).
#' @param id_var Column with ID; unquoted (e.g., `id`) or character (e.g., `"id"`).
#' @param group_var Optional grouping column(s) (e.g., age band, gender);
#'   unquoted or character. Use `NULL` if none.
#' @param kcd_var Column with KCD codes; unquoted or character.
#' @param from_var Start date column of each episode; unquoted or character.
#' @param to_var End date column of each episode; unquoted or character.
#' @param uw_date Underwriting date used as time zero; `"YYYY-MM-DD"`, `Date`, `POSIXt`,
#'   or a column in `cohort` (unquoted or character).
#' @param ... One or more KCD term windows, each as
#'   `list(start_months, end_months, pattern)` where `start_months` is
#'   months **before** `uw_date` (negative), `end_months` is months
#'   **after** `uw_date` (positive), and `pattern` is a KCD code or regex
#'   (e.g., `"E1[0-4]"`). Optionally give each list a name to control
#'   the output column name.
#'
#' @details
#' - Produces one row per ID(+group), with a 0/1 flag column for each KCD term.
#' - Named `...` arguments become the column names; unnamed terms default to
#'   `"pattern_start_end"`.
#' - Also attaches summary tables as attributes:
#'   * `attr(x, "raw")`: lowest-level contingency table
#'   * `attr(x, "summary")`: first-level summary (alias of `"summary.1"`)
#'   * `attr(x, "summary.N")`: deeper summaries by nested groupings
#' - The main result is restored to the original class of `cohort` and prepended
#'   with S3 class `"ir.data"`.
#'
#' @return An object of the same base class as `cohort` with prepended class
#'   `"ir.data"`. Contains one row per ID(+group) and 0/1 KCD flag columns.
#'
#' @examples
#' \dontrun{
#' # Example with unquoted columns
#' res <- summarise_id_with_kcd(
#'   cohort    = cohort,
#'   id_var    = id,
#'   group_var = age_band,
#'   kcd_var   = kcd,
#'   from_var  = sdate,
#'   to_var    = edate,
#'   uw_date   = "2017-08-01",
#'   hypertension = list(-60, 0,  "I10"),
#'   cv_event     = list(  0, 36, "I2[0-5]|I6[0-9]|G46")
#' )
#' head(res)
#'
#' # Example with character column names
#' res2 <- summarise_id_with_kcd(
#'   cohort    = cohort,
#'   id_var    = "id",
#'   group_var = "age_band",
#'   kcd_var   = "kcd",
#'   from_var  = "sdate",
#'   to_var    = "edate",
#'   uw_date   = "2017-08-01",
#'   list(-60, 0, "I10")
#' )
#' }
#'
#' @seealso [subset_cohort_by_id_with_kcd()], [instead::ensure_dt_env()], [instead::capture_names()]
#'
#' @export
summarise_id_with_kcd <- function(cohort, id_var, group_var, kcd_var,
                                  from_var, to_var, uw_date, ...) {
  # class: ir.data (incidence rate data)
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt <- env$dt

  id_var    <- instead::capture_names(dt, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(dt, !!rlang::enquo(group_var))
  kcd_var   <- instead::capture_names(dt, !!rlang::enquo(kcd_var))
  from_var  <- instead::capture_names(dt, !!rlang::enquo(from_var))
  to_var    <- instead::capture_names(dt, !!rlang::enquo(to_var))
  uw_date   <- .resolve_uw_date(dt, !!rlang::enquo(uw_date))
  kcd_terms <- rlang::list2(...)

  id_group_var <- c(id_var, group_var)

  n <- length(kcd_terms)
  id_list <- vector(mode = "list", length = n + 1L)
  id_list[[1L]] <- unique(dt[, .SD, .SDcols = id_group_var])
  id_list[2L:length(id_list)] <- lapply(
    seq_along(kcd_terms),
    function(x) {
      start  <- kcd_terms[[x]][[1L]]
      end    <- kcd_terms[[x]][[2L]]
      kcd_cd <- kcd_terms[[x]][[3L]]
      ds <- subset_cohort_by_id_with_kcd(
        dt, id_var = id_var, kcd_var = kcd_var,
        from_var = from_var, to_var = to_var,
        uw_date, start, end, !!!kcd_cd # essential !!!
      )
      col <- sprintf("%s_%s_%s", paste(kcd_cd, collapse = "+"), start, end)
      ds <- ds[, list(dia = 1), keyby = id_group_var]
      data.table::setnames(ds, "dia", col)
    }
  )
  z <- Reduce(function(...) merge(..., by = id_group_var, all = TRUE), id_list)
  instead::replace_na_with_zero(z)

  if (!is.null(names(kcd_terms))) {
    new_names <- c(id_group_var, names(kcd_terms))
    idx <- which(new_names == "")
    new_names[idx] <- names(z)[idx]
    data.table::setnames(z, new_names)
  }

  cols <- setdiff(names(z), id_var)
  zs <- z[, list(n = .N), by = cols]
  zs[, `:=`((cols), lapply(.SD, factor)), .SDcols = cols]
  data.table::setorderv(zs, cols)
  raw <- data.table::copy(zs)
  raw <- env$restore(raw)
  data.table::setattr(z, "raw", raw)
  ratio <- n <- nsum <- NULL
  for (i in 1L:(length(cols) - 1L)) {
    grp <- cols[1L:(length(cols) - i)]
    zs[, `:=`(nsum, sum(n)), by = grp]
    zs[, `:=`(ratio, n / nsum)]
    smr <- data.table::copy(zs)
    smr <- env$restore(smr)
    data.table::setattr(z, paste0("summary.", i), smr)
  }
  smr <- data.table::copy(attr(z, "summary.1"))
  data.table::setattr(z, "summary", smr)
  z <- env$restore(z)
  z <- instead::prepend_class(z, "ir.data")

  z
}

#' Summarise IDs by KCD terms to support "Incidence Rate" calculation (class-preserving)
#'
#' From an episode-level cohort (multiple rows per ID with `[from, to]` and `kcd`),
#' produce an **ID-level summary table** with 0/1 flags for each specified KCD
#' window around `uw_date`. Internally converts to data.table (via
#' [instead::ensure_dt_env()]) for speed, then restores both the main result and the
#' summaries to the original base class of `df` (data.frame / tibble / data.table).
#' The main result prepends S3 class `"ir.data"`, the summary prepends `"ir"`.
#'
#' @param cohort A cohort data frame at **episode level** (multiple rows per ID).
#' @param id_var ID column; unquoted (e.g., `id`) or character (e.g., `"id"`).
#' @param group_var Optional grouping column (e.g., age band, gender);
#'   unquoted or character. Use `NULL` for no grouping.
#' @param kcd_var Column with KCD codes; unquoted or character.
#' @param from_var Start date column of each episode; unquoted or character.
#' @param to_var End date column of each episode; unquoted or character.
#' @param uw_date Underwriting date used as time zero; `"YYYY-MM-DD"`, `Date`, `POSIXt`,
#'   or a column in `cohort` (unquoted or character).
#' @param decl,decl2,decl3 Optional declaration windows as lists of the form
#'   `list(start_months, end_months, pattern)` where `start_months` is months
#'   **before** `uw_date` (negative), `end_months` is months **after** `uw_date`
#'   (positive), and `pattern` is a KCD code or regex (e.g., `"E1[0-4]"`).
#' @param excl Optional exclusion window, same list form.
#' @param claim Optional claim window, same list form.
#'
#' @details
#' - Column arguments accept both unquoted and character inputs; they are
#'   normalised with `instead::capture_names()` against the working `data.table`.
#' - Flags and summaries are computed by `summarise_id_with_kcd()` and then
#'   post-processed: multiple `decl*` columns can be collapsed into a single
#'   `"decl"` factor for compactness.
#' - Attributes attached:
#'   * `attr(x, "summary")`: first-level summary (restored, class-prepended `"ir"`).
#'   * `attr(x, "summary.N")`: deeper summaries, if present (restored).
#'   * `attr(x, "raw")`: contingency table at the lowest level, if present (restored).
#'   * On the summary: `"decl"`, `"excl"`, `"claim"` strings if provided.
#'
#' @return An object of the same base class as `df`, with prepended class `"ir.data"`.
#'   One row per ID(+group) and 0/1 KCD flag columns. A restored summary is available
#'   at `attr(result, "summary")` (class-prepended `"ir"`).
#'
#' @examples
#' \dontrun{
#' res <- summarise_id_with_kcd_ir(
#'   cohort    = cohort,
#'   id_var    = id,
#'   group_var = age_band,
#'   kcd_var   = kcd,
#'   from_var  = sdate,
#'   to_var    = edate,
#'   uw_date   = "2018-07-01",
#'   decl      = list(-60, 0,  "I10"),
#'   decl2     = list(-60, 0,  "E78"),
#'   excl      = list(-60, 0,  "I2[0-5]|I6[0-9]|G46"),
#'   claim     = list(  0, 36, "I2[0-5]|I6[0-9]|G46")
#' )
#' class(res)          # includes "ir.data"
#' class(summary(res)) # includes "ir"
#' }
#'
#' @seealso [summarise_id_with_kcd()], [instead::ensure_dt_env()], [instead::capture_names()]
#'
#' @export
summarise_id_with_kcd_ir <- function(cohort, id_var, group_var, kcd_var,
                                     from_var, to_var, uw_date,
                                     decl = NULL, decl2 = NULL, decl3 = NULL,
                                     excl = NULL, claim = NULL) {
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt <- env$dt

  id_var    <- instead::capture_names(dt, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(dt, !!rlang::enquo(group_var))
  kcd_var   <- instead::capture_names(dt, !!rlang::enquo(kcd_var))
  from_var  <- instead::capture_names(dt, !!rlang::enquo(from_var))
  to_var    <- instead::capture_names(dt, !!rlang::enquo(to_var))
  uw_date   <- .resolve_uw_date(dt, uw_date)

  id_group_var <- c(id_var, group_var)

  decl1 <- decl; rm(decl)

  dots <- rlang::list2(
    decl1 = decl1, decl2 = decl2, decl3 = decl3, excl = excl, claim = claim
  )
  dots <- dots[sapply(dots, function(x) !is.null(x))]

  ds <- summarise_id_with_kcd(
    dt, id_var = id_var, group_var = group_var,
    kcd_var = kcd_var, from_var = from_var, to_var = to_var,
    uw_date = uw_date, !!!dots  # essential !!!
  )
  smr <- summary(ds)
  decl_cols <- names(smr)[grepl("^decl", names(smr), perl = TRUE)]
  smr$decl <- instead::paste_cols(smr, decl_cols, sep = "+")
  smr$decl <- as.factor(smr$decl)
  instead::rm_cols(smr, decl_cols)
  data.table::setcolorder(smr, "decl", before = "excl")
  decl <- list(decl1[[3L]], decl2[[3L]], decl3[[3L]])
  decl <- decl[sapply(decl, function(x) !is.null(x))]

  ds <- env$restore(ds)
  ds <- instead::prepend_class(ds, "ir.data")

  smr <- env$restore(smr)
  smr <- instead::prepend_class(smr, "ir")
  data.table::setattr(smr, "decl", paste(decl, collapse = " & "))
  data.table::setattr(smr, "excl", excl[[3L]])
  data.table::setattr(smr, "claim", claim[[3L]])
  data.table::setattr(ds, "summary", smr)

  smr_i_list <- instead::regex_attr(ds, "summary_\\d")
  for (smr_i in smr_i_list) {
    ds_smr_i <- attr(ds, smr_i)
    ds_smr_i <- env$restore(ds_smr_i)
    data.table::setattr(ds, smr_i, ds_smr_i)
  }

  raw <- attr(ds, "raw")
  raw <- env$restore(raw)
  data.table::setattr(ds, "raw", raw)

  ds
}

#' @method summary ir.data
#' @export
summary.ir.data <- function(object, ...) {
  attr(object, "summary")
}

#' Get relative risks (core engine)
#'
#' Compute RR/OR and their confidence intervals from an `ir` object.
#' This is the internal engine used by S3 methods.
#'
#' @param x An `ir` object (for `ir.data`, see methods below).
#' @param decl_vs Character vector of length 2 giving the declaration
#'   levels to compare (e.g., `c("0","1")`).
#' @param conf.level Confidence level for intervals (default `0.95`).
#'
#' @return See [summarise_rr()] for columns and attributes. Returns an object
#'   with class `"rr"`.
#'
#' @keywords internal
#' @export
.summarise_rr_core <- function(x, decl_vs = c("0", "1"), conf.level = .95) {
  env <- instead::ensure_dt_env(x)
  dt  <- env$dt

  decl_levels <- levels(dt$decl)
  if (length(setdiff(decl_vs, decl_levels)) > 0)
    stop("Invalid declaration levels", call. = FALSE)

  ds <- dt[dt$decl %chin% decl_vs & dt$excl == 0 & dt$claim == 1,]

  if (instead::unilen(ds$decl) > 2)
    stop("Please select two types of declarations to calculate relative risk")

  groups <- instead::anti_cols(
    ds, c("decl", "excl", "claim", "n", "nsum", "ratio")
  )
  if (length(groups) == 0) groups <- "."

  fml <- stats::formula(sprintf("%s ~ decl", paste(groups, collapse = " + ")))

  dn  <- data.table::dcast(ds, fml, value.var = "n", fun.aggregate = sum)
  instead::replace_na_with_zero(dn)
  data.table::setnames(dn, c(groups, c("n01", "n11")))

  dm <- data.table::dcast(ds, fml, value.var = "nsum", fun.aggregate = sum)
  instead::replace_na_with_zero(dm)
  data.table::setnames(dm, c(groups, c("nsum0", "nsum1")))

  n01 <- n11 <- NULL
  dm[dn, on = groups, `:=`(n01 = n01, n11 = n11)]
  data.table::set(dm, j = "n00", value = dm$nsum0 - dm$n01)
  data.table::set(dm, j = "n10", value = dm$nsum1 - dm$n11)
  instead::rm_cols(dm, c("nsum0", "nsum1"))
  data.table::setnames(
    dm,
    c("n11", "n01", "n10", "n00"),
    c("tp" , "fn" , "fp" , "tn" )
  )
  data.table::setcolorder(dm, c(groups, "tp", "fn", "fp", "tn"))

  # contingency matrix
  arr <- array(t(as.matrix(
    dm[, c("tp", "fn", "fp", "tn")]
  )), dim = c(2L, 2L, nrow(dm)))
  each_names <- do.call(
    paste, c(dm[, .SD, drop = FALSE, .SDcols = groups], sep = " / ")
  )
  dimnames(arr) <- list(
    c("exposed+", "exposed-"),
    c("outcome+", "outcome-"),
    each_names
  )
  arr <- stats::addmargins(arr, c(1L, 2L))
  dimnames(arr) <- lapply(dimnames(arr), tolower)
  cmatrix <- lapply(seq_len(dim(arr)[3]), function(i) {
    m <- arr[,, i, drop = FALSE]
    dim(m) <- dim(arr)[1:2]
    dimnames(m) <- dimnames(arr)[1:2]
    m
  })
  cmatrix <- lapply(seq_along(cmatrix), function(i) {
    cbind(cmatrix[[i]], .binom_test(cmatrix[[i]][,1], cmatrix[[i]][,3])[, 3:5])
  })
  names(cmatrix) <- dimnames(arr)[[3]]

  # incidence
  data.table::set(dm, j = "inc0", value = dm$fn / (dm$fn + dm$tn))
  data.table::set(dm, j = "inc1", value = dm$tp / (dm$tp + dm$fp))

  # relative risk
  rr_res <- .rr_ci_wald(
    tp = dm$tp, fn = dm$fn, fp = dm$fp, tn = dm$tn,
    conf.level = conf.level
  )

  # odds ratio
  or_res <- .or_ci_fisher(
    tp = dm$tp, fn = dm$fn, fp = dm$fp, tn = dm$tn,
    conf.level = conf.level
  )

  dm[, `:=`(
    rr        = rr_res$rr,
    rr_lower  = rr_res$lower,
    rr_upper  = rr_res$upper,
    or        = or_res$or,
    or_lower  = or_res$lower,
    or_upper  = or_res$upper,
    p_value   = or_res$p_value,
    decision  = factor(ifelse(or_res$p_value < (1 - conf.level), "reject", "fail"),
                       levels = c("reject", "fail"))
  )]

  data.table::setattr(dm, "decl" , attr(x, "decl"))
  data.table::setattr(dm, "excl" , attr(x, "excl"))
  data.table::setattr(dm, "claim", attr(x, "claim"))
  data.table::setattr(dm, "cmatrix", cmatrix)

  dm <- instead::prepend_class(dm, "rr")
  dm <- env$restore(dm)

  dm
}

#' Summarise relative risk from IR objects
#'
#' Compute relative risk (RR) and odds ratio (OR) with confidence intervals
#' from `ir` / `ir.data` objects by comparing two declaration levels.
#'
#' Internally, counts are reshaped into 2×2 contingency tables per stratum,
#' RR CIs are computed by a Wald method, and OR CIs and p-values by Fisher's
#' exact test. Results are returned per stratum with attributes describing the
#' declaration/exclusion/claim patterns and the contingency array.
#'
#' @param x An `ir` or `ir.data` object.
#' @param ... Passed to class-specific methods.
#'
#' @return A data frame (same base class as the input's restored table) with
#'   prepended class `"rr"`. Columns include:
#'   - `tp, fn, fp, tn`: 2×2 cell counts,
#'   - `inc0, inc1`: incidence in reference vs. exposed groups,
#'   - `rr, rr_lower, rr_upper`: RR and its CI (Wald),
#'   - `or, or_lower, or_upper`: OR and its CI (Fisher),
#'   - `p_value`: Fisher exact p-value,
#'   - `decision`: factor `"reject"` / `"fail"` for `H0: OR = 1`.
#'
#'   Attributes:
#'   - `attr(x, "decl")`, `attr(x, "excl")`, `attr(x, "claim")`,
#'   - `attr(x, "cmatrix")`: a 2×2×K contingency array with dimnames
#'     `c("exp+","exp-")`, `c("out+","out-")`, and per-stratum labels.
#'
#' @examples
#' \dontrun{
#' # x: an 'ir' object created upstream
#' out <- summarise_rr(x, decl_vs = c("0","1"), conf.level = 0.95)
#' head(out)
#' attr(out, "cmatrix")
#' }
#'
#' @export
summarise_rr <- function(x, ...) {
  UseMethod("summarise_rr")
}

#' @rdname summarise_rr
#' @param decl_vs Character vector of two declaration levels to compare (default `c("0","1")`).
#' @param conf.level Confidence level for intervals (default `0.95`).
#' @export
summarise_rr.ir <- function(x, decl_vs = c("0","1"), conf.level = .95, ...) {
  instead::assert_class(x, "ir")
  .summarise_rr_core(x, decl_vs = decl_vs, conf.level = conf.level)
}

#' @rdname summarise_rr
#' @export
summarise_rr.ir.data <- function(x, decl_vs = c("0","1"), conf.level = .95, ...) {
  instead::assert_class(x, "ir.data")
  .summarise_rr_core(summary(x), decl_vs = decl_vs, conf.level = conf.level)
}

#' Incidence Rate Plot
#'
#' Draw an incidence rate plot.
#'
#' @param x ir object.
#' @param palette A pair palette key for two-level fills: `"base"` or `"deep"`.
#' @param scales Facet scales when faceting by `gender`: `"fixed"` (default),
#'   `"free"`, `"free_x"`, or `"free_y"`.
#' @param theme A theme key for `ggshort::switch_theme()`: `"view"`, `"save"`, `"shiny"`.
#' @param ... Passed to `ggshort::switch_theme()`.
#'
#' @return A ggplot object.
#' @export
plot_ir <- function(x,
                    palette = c("base", "deep"),
                    scales = c("fixed", "free_y", "free_x", "free"),
                    theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "ir")

  palette <- match.arg(palette)
  scales  <- match.arg(scales)
  theme   <- match.arg(theme)

  decl  <- attr(x, "decl")
  excl  <- attr(x, "excl")
  claim <- attr(x, "claim")
  subtitle <- sprintf("Decl: %s - Excl: %s - Claim: %s", decl, excl, claim)
  title    <- "Incidence Rate"

  data <- x[x$excl == 0 & x$claim == 1,]
  is_pair <- length(levels(data$decl)) <= 2

  # Decide x variable & facet dynamically
  has_gender   <- instead::has_cols(data, "gender")
  has_age_band <- instead::has_cols(data, "age_band")

  x_var <- if (has_age_band) "age_band" else if (has_gender) "gender" else "decl"
  x_angle <- if (x_var == "age_band") 90 else 0

  # base plot
  p <- ggshort::ggbar(
    data = data,
    x    = .data[[x_var]],
    y    = .data$ratio,
    fill = .data$decl
  ) +
    # apply two-level palette only when decl has <= 2 levels
    list(if (is_pair) ggshort::scale_fill_pair_manual(palette = palette)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggshort::switch_theme(theme = theme, x.angle = x_angle, y.size = 0,
                          legend.position = "bottom",
                          legend.justification = c(1, 0))

  # Facet only when both gender and age_band exist (original behavior)
  if (has_gender && has_age_band) {
    p <- p + ggplot2::facet_wrap(~ gender, scales = scales)
  }

  p
}

#' @method plot ir
#' @export
plot.ir <- function(x, color_type = c("base", "deep"),
                    scales = c("fixed", "free_y", "free_x", "free"),
                    theme = c("view", "save", "shiny"), ...) {
  color_type <- match.arg(color_type)
  scales     <- match.arg(scales)
  theme      <- match.arg(theme)
  plot_ir(x = x, color_type = color_type, scales = scales, theme = theme)
}

#' Relative Risk Plot
#'
#' Visualize relative risks from an `rr` object in a compact, consistent style
#' (mirrors the structure of `plot_ir`).
#'
#' @param x An object of class `"rr"`.
#' @param logscale Logical; if `TRUE`, use a log scale on the y-axis.
#' @param scales Facet scales (used when faceting by `gender`):
#'   one of `"fixed"` (default), `"free"`, `"free_x"`, or `"free_y"`.
#' @param theme Theme key for [ggshort::switch_theme()]:
#'   one of `"view"` (default), `"save"`, or `"shiny"`.
#' @param palette Pair palette key for two-level fills. One of `"base"` or `"deep"`.
#'   Applied only when `decl` has <= 2 levels.
#' @param ... Additional arguments passed to [ggshort::switch_theme()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' p <- plot_rr(rr)  # base plot
#' p <- plot_rr(rr, logscale = TRUE, palette = "deep")
#' }
#' @export
plot_rr <- function(x,
                    logscale = FALSE,
                    palette  = c("base", "deep"),
                    scales   = c("fixed", "free_y", "free_x", "free"),
                    theme    = c("view", "save", "shiny"),
                    ...) {
  instead::assert_class(x, "rr")

  scales  <- match.arg(scales)
  theme   <- match.arg(theme)
  palette <- match.arg(palette)

  # Titles / subtitles from attributes
  decl     <- attr(x, "decl")
  excl     <- attr(x, "excl")
  claim    <- attr(x, "claim")
  title    <- if (logscale) "Relative Risk (log scale)" else "Relative Risk"
  subtitle <- sprintf("Decl: %s - Excl: %s - Claim: %s", decl, excl, claim)

  # Text label (N.S. if decision == "fail") — keep existing semantics
  if (!has_cols(x, "label"))
    x$label <- sprintf(
      "%s\n%.2f\n(%s)",
      ifelse(x$decision == "fail", "N.S.", ""),
      x$rr, x$tp
    )

  # Dynamic x-aes & facet rule
  has_gender   <- instead::has_cols(x, "gender")
  has_age_band <- instead::has_cols(x, "age_band")

  x_var   <- if (has_age_band) "age_band" else if (has_gender) "gender" else "."
  x_angle <- if (identical(x_var, "age_band")) 90 else 0

  # ymax for label headroom (optional but handy)
  ymax <- max(x$rr, na.rm = TRUE) * 1.3

  # decl 2-level pair palette
  # is_pair <- length(unique(x$decl)) <= 2
  # fill_var <- if (has_gender) "gender" else NULL

  # base plot (one-pass assemble)
  p <- ggshort::ggbar(
    data = x,
    x    = .data[[x_var]],
    y    = .data$rr,
    # fill = .data[[fill_var]],
    ymax = ymax,
    label = .data$label,
    label_args = list(size = 3, vjust = -.25)
  ) +
    # log-scale (optional)
    list(if (logscale) ggplot2::scale_y_log10()) +
    # two-level pair palette (optional)
    # list(if (is_pair) ggshort::scale_fill_gender(palette = palette)) +
    # facet rule: only when both gender & age_band exist
    list(if (has_gender && has_age_band)
      ggplot2::facet_wrap(~ gender, scales = scales)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  caption = "N.S.: Not Significant") +
    ggshort::switch_theme(theme = theme, x.angle = x_angle, y.size = 0)

  p
}

#' @method plot rr
#' @export
plot.rr <- function(x,
                    logscale = FALSE,
                    palette  = c("base", "deep"),
                    scales   = c("fixed", "free_y", "free_x", "free"),
                    theme    = c("view", "save", "shiny"),
                    ...) {
  plot_rr(
    x        = x,
    logscale = logscale,
    scales   = match.arg(scales),
    theme    = match.arg(theme),
    palette  = match.arg(palette),
    ...
  )
}

#' @export
save_rr_xlsx <- function(ir, rr, mix, file = "RR.xlsx", sheet = "RR", overwrite = FALSE) {
  instead::assert_class(ir, "ir")
  instead::assert_class(rr, "rr")
  instead::assert_class(mix, "data.frame")

  cmatrix <- attr(rr, "cmatrix")
  cmatrix <- lapply(cmatrix, function(m) m[, 1L:3L])

  # Workbook
  wb <- if (file.exists(file)) {
    openxlsx::loadWorkbook(file)
  } else {
    openxlsx::createWorkbook()
  }

  # 2x2 matrix
  wb <- instead::save_data_wb(
    data = cmatrix,
    wb = wb,
    sheet = sheet,
    rc = c(2, 2),
    row_space = 3,
    row_names = TRUE,
    font_size = 11,
    title_size = 11,
    auto_width = FALSE,
    num_fmt = c(
      "outcome+" = "#,##0",
      "outcome-" = "#,##0",
      "sum"      = "#,##0",
      "estimate" = "0.00",
      "upper"    = "0.00",
      "lower"    = "0.00"
    )
  )

  # RR
  res <- rr[, .(rr, lower = rr_lower, upper = rr_upper, p_value)]
  res <- unname(split(res, seq_len(nrow(res))))

  wb <- instead::save_data_wb(
    data = res,
    wb = wb,
    sheet = sheet,
    rc = c(7L, 2L),
    row_space = 6,
    row_names = FALSE,
    font_size = 11,
    title_size = 11,
    auto_width = FALSE,
    num_fmt = c(
      "rr"      = "0.00",
      "lower"   = "0.00",
      "upper"   = "0.00",
      "p_value" = "0.00"
    )
  )

  # loadings
  loadings <- rr[!is.nan(rr), .(age_band, rr, lower = rr_lower, upper = rr_upper, p_value, decision)]
  loadings[, loading := rr - 1]
  loadings[mix, on = .(age_band), wt := wt]

  loadings_list <- lapply(1:(nrow(loadings)-1), function(i) {
    loading <- data.table::as.data.table(loadings[i:nrow(loadings)])
    loading[, wt := wt / sum(wt)]
    loading[, wt_loading := rr * wt]
  })

  .get_age_band_range <- function(x) {
    if (is.factor(x))
      x <- as.character(sort(x))
    rng <- range(x)
    start <- strsplit(rng[[1L]], split = "-")[[1L]][1L]
    end   <- strsplit(rng[[2L]], split = "-")[[1L]][2L]
    paste0(start, "-", end)
  }

  titles <- unlist(lapply(loadings_list, function(df) .get_age_band_range(df$age_band)))

  wb <- instead::save_data_wb(
    data = loadings_list,
    wb = wb,
    sheet = sheet,
    rc = list(c(2, 7)),
    font_size = 11,
    data_title = titles,
    title_size = 11,
    num_fmt = "0.00",
    auto_width = FALSE
  )

  # plots
  p_ir <- plot_ir(ir, palette = "base")
  p_rr <- plot_rr(rr, palette = "base")
  p_list <- list("Incidence Rate" = p_ir, "Relative Risk" = p_rr)

  ggshort::suppress_geom_removed_warnings({
    wb <- instead::save_plot_wb(
      plot = p_list,
      wb = wb,
      sheet = sheet,
      rc = c(2, 17),
      rows_per_inch = 4
    )
  })

  openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)

  file
}

# Internal helper functions -----------------------------------------------

.resolve_uw_date <- function(dt, uw_date) {
  uw_quo  <- rlang::enquo(uw_date)
  uw_col <- tryCatch(
    instead::capture_names(dt, !!uw_quo),
    error = function(e) NULL
  )

  if (!is.null(uw_col)) return(dt[[uw_col]])

  uw_val <- rlang::eval_tidy(uw_quo)
  uw_val <- if (inherits(uw_val, "Date")) uw_val else as.Date(uw_val)
  uw_val[1L]
}

#' Wald confidence interval for Relative Risk (RR)
#'
#' Compute point estimate, Wald-type confidence interval, and Wald p-value
#' for the relative risk (RR) from a 2x2 outcome table.
#'
#' @param tp Count of `exposed+` and `outcome+`.
#' @param fn Count of `exposed-` and `outcome+`.
#' @param fp Count of `exposed+` and `outcome-`.
#' @param tn Count of `exposed-` and `outcome-`.
#' @param conf.level Confidence level for the interval (default 0.95).
#' @param alternative Alternative hypothesis: `"two.sided"` (default),
#'   `"greater"` or `"less"`.
#' @param correction Logical; if `TRUE`, applies Haldane–Anscombe 0.5
#'   correction to zero cells (for SE stability).
#'
#' @return A list with numeric vectors: `RR`, `lower`, `upper`, and `p_value`.
#' @keywords internal
.rr_ci_wald <- function(tp, fn, fp, tn, conf.level = .95,
                        alternative = c("two.sided", "greater", "less"),
                        correction = FALSE) {
  alternative <- match.arg(alternative)

  # length check
  if (!all(length(tp) == length(fn) &
           length(fn) == length(fp) &
           length(fp) == length(tn))) {
    stop("tp, fn, fp, tn must have the same length")
  }

  # point estimate
  n1 <- tp + fp
  n0 <- fn + tn
  rr <- (tp / n1) / (fn / n0)
  log_rr <- log(rr)

  # Haldane–Anscombe correction (only if requested)
  if (correction) {
    tp <- ifelse(tp == 0, tp + 0.5, tp)
    fn <- ifelse(fn == 0, fn + 0.5, fn)
    fp <- ifelse(fp == 0, fp + 0.5, fp)
    tn <- ifelse(tn == 0, tn + 0.5, tn)
  }

  n1 <- tp + fp
  n0 <- fn + tn

  se_log_rr <- sqrt(1 / tp - 1 / n1 + 1 / fn - 1 / n0)

  # critical z
  z_crit <- if (alternative == "two.sided") {
    qnorm((1 + conf.level) / 2)
  } else {
    qnorm(conf.level)
  }

  # CI
  if (alternative == "two.sided") {
    lower <- exp(log_rr - z_crit * se_log_rr)
    upper <- exp(log_rr + z_crit * se_log_rr)
  } else if (alternative == "greater") {
    lower <- exp(log_rr - z_crit * se_log_rr)
    upper <- rep(Inf, length(lower))
  } else { # "less"
    lower <- rep(0, length(log_rr))
    upper <- exp(log_rr + z_crit * se_log_rr)
  }

  # Wald p-value (H0: rr = 1)
  z_stat <- log_rr / se_log_rr
  p_value <- switch(alternative,
                   "two.sided" = 2 * (1 - pnorm(abs(z_stat))),
                   "greater"   = 1 - pnorm(z_stat),
                   "less"      = pnorm(z_stat))

  lower[!is.finite(lower)] <- 0
  list(rr = rr, lower = lower, upper = upper, p_value = p_value)
}

#' Wald confidence interval for Odds Ratio (OR)
#'
#' Compute point estimate, Wald-type confidence interval, and Wald p-value
#' for the odds ratio (OR) from a 2x2 outcome table.
#'
#' @inheritParams .rr_ci_wald
#'
#' @return A list with numeric vectors: `OR`, `lower`, `upper`, and `p_value`.
#' @keywords internal
.or_ci_wald <- function(tp, fn, fp, tn, conf.level = .95,
                        alternative = c("two.sided", "greater", "less"),
                        correction = FALSE) {
  alternative <- match.arg(alternative)

  # input length check
  if (!all(length(tp) == length(fn) &
           length(fn) == length(fp) &
           length(fp) == length(tn))) {
    stop("tp, fn, fp, tn must have the same length")
  }

  # Apply Haldane–Anscombe correction if requested
  if (correction) {
    tp <- tp + 0.5
    fn <- fn + 0.5
    fp <- fp + 0.5
    tn <- tn + 0.5
  }

  # Odds ratio and log OR
  or    <- (tp * tn) / (fp * fn)
  log_or <- log(or)

  # Standard error
  se_log_or <- sqrt(1/tp + 1/fp + 1/fn + 1/tn)

  # critical z
  z_crit <- if (alternative == "two.sided") {
    qnorm((1 + conf.level) / 2)
  } else {
    qnorm(conf.level)
  }

  # CI by alternative
  if (alternative == "two.sided") {
    lower <- exp(log_or - z_crit * se_log_or)
    upper <- exp(log_or + z_crit * se_log_or)
  } else if (alternative == "greater") {
    lower <- exp(log_or - z_crit * se_log_or)
    upper <- rep(Inf, length(lower))
  } else { # "less"
    lower <- rep(0, length(log_or))
    upper <- exp(log_or + z_crit * se_log_or)
  }

  # Wald p-value for H0: OR = 1
  z_stat <- log_or / se_log_or
  p_value <- switch(
    alternative,
    "two.sided" = 2 * (1 - pnorm(abs(z_stat))),
    "greater"   = 1 - pnorm(z_stat),
    "less"      = pnorm(z_stat)
  )

  # tidy up possible numerical issues
  lower[!is.finite(lower)] <- 0

  list(or = or, lower = lower, upper = upper, p_value = p_value)
}

#' Wald confidence interval for Relative Risk (RR)
#'
#' Compute point estimate, Wald-type confidence interval, and Wald p-value
#' for the relative risk (RR) from a 2x2 outcome table.
#'
#' @param tp Count of `exposed+` and `outcome+`.
#' @param fn Count of `exposed-` and `outcome+`.
#' @param fp Count of `exposed+` and `outcome-`.
#' @param tn Count of `exposed-` and `outcome-`.
#' @param conf.level Confidence level for the interval (default 0.95).
#' @param alternative Alternative hypothesis: `"two.sided"` (default),
#'   `"greater"` or `"less"`.
#' @param correction Logical; if `TRUE`, applies Haldane–Anscombe 0.5
#'   correction to zero cells (for SE stability).
#'
#' @return A list with numeric vectors: `RR`, `lower`, `upper`, and `p_value`.
#' @keywords internal
.or_ci_fisher <- function(tp, fn, fp, tn, conf.level = .95,
                          alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  # Check that all input vectors have the same length
  if (!all(length(tp) == length(fn) &
           length(fn) == length(fp) &
           length(fp) == length(tn))) {
    stop("`tp`, `fn`, `fp`, `tn` must have the same length")
  }

  n <- length(tp)

  # Pre-allocate numeric vectors for results
  or      <- numeric(n)
  lower   <- numeric(n)
  upper   <- numeric(n)
  p_value  <- numeric(n)

  for (i in seq_len(n)) {
    # Construct a 2x2 contingency matrix
    m <- matrix(c(tp[i], fp[i], fn[i], tn[i]), nrow = 2L, byrow = TRUE)

    # Run Fisher's exact test on the 2x2 table
    ft <- stats::fisher.test(
      m,
      conf.level  = conf.level,
      alternative = alternative
    )

    # Extract odds ratio (estimate), confidence interval, and p-value
    or[i]     <- unname(ft$estimate[[1L]])
    lower[i]  <- ft$conf.int[1L]
    upper[i]  <- ft$conf.int[2L]
    p_value[i] <- ft$p.value
  }

  # Return results as a list of numeric vectors
  list(or = or, lower = lower, upper = upper, p_value = p_value)
}

.binom_test <- function(x, n, p = .5,
                        alternative = c("two.sided", "less", "greater"),
                        conf.level = .95) {
  alternative <- match.arg(alternative)

  stopifnot(length(x) == length(n))

  out <- data.frame(
    x = x,
    n = n,
    estimate = NA_real_,
    lower = NA_real_,
    upper = NA_real_
  )

  for (i in seq_along(x)) {
    if (n[i] == 0) {
      out$estimate[i] <- NaN
      out$lower[i]    <- NaN
      out$upper[i]    <- NaN
    } else {
      bt <- binom.test(x[i], n[i], p = p,
                       alternative = alternative,
                       conf.level = conf.level)
      out$estimate[i] <- bt$estimate
      out$lower[i]    <- bt$conf.int[1]
      out$upper[i]    <- bt$conf.int[2]
    }
  }

  out
}

.build_cmatrix <- function(df, group_var) {
  instead::assert_cols(df, c("tp", "fn", "fp", "tn"))

  arr <- array(t(as.matrix(
    df[, c("tp", "fn", "fp", "tn")]
  )), dim = c(2L, 2L, nrow(df)))

  each_names <- instead::paste_cols(df, group_var, sep = " / ")

  dimnames(arr) <- list(
    c("exposed+", "exposed-"),
    c("outcome+", "outcome-"),
    each_names
  )
  arr <- stats::addmargins(arr, c(1L, 2L))
  dimnames(arr) <- lapply(dimnames(arr), tolower)
  cmatrix <- lapply(seq_len(dim(arr)[3]), function(i) {
    m <- arr[,, i, drop = FALSE]
    dim(m) <- dim(arr)[1:2]
    dimnames(m) <- dimnames(arr)[1:2]
    m
  })
  cmatrix <- lapply(seq_along(cmatrix), function(i) {
    cbind(cmatrix[[i]], .binom_test(cmatrix[[i]][,1], cmatrix[[i]][,3])[, 3:5])
  })
  names(cmatrix) <- dimnames(arr)[[3]]
}
