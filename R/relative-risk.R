
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

# __________________________________________ ------------------------------

# Cohort Summary ----------------------------------------------------------

#' Summarise IDs with KCD terms
#'
#' Starting from an *episode-level* cohort (multiple rows per ID with
#' `[from,to]` and `kcd`), produce an **ID-level summary table** with
#' 0/1 flag columns for each specified KCD window relative to `uw_date`.
#' Internally converts to data.table (via [instead::ensure_dt_env()]) for
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
#'   with S3 class `"id_data"`.
#'
#' @return An object of the same base class as `cohort` with prepended class
#'   `"id_data"`. Contains one row per ID(+group) and 0/1 KCD flag columns.
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
#' @seealso [subset_cohort_by_id_with_kcd()]
#'
#' @export
summarise_id_with_kcd <- function(cohort, id_var, group_var, kcd_var,
                                  from_var, to_var, uw_date, ...) {
  # class: id_data
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

  cols <- instead::anti_cols(z, id_var)
  zs <- z[, list(n = .N), by = cols]

  cols_factor <- instead::anti_cols(z, id_group_var)
  instead::set_col_map(zs, cols_factor, factor)
  data.table::setorderv(zs, cols)

  raw <- data.table::copy(zs)
  raw <- env$restore(raw)
  data.table::setattr(z, "raw", raw)

  data.table::setattr(z, "summary", NA) # allocate in advance

  ratio <- n <- nsum <- NULL
  n_cols <- length(cols)
  for (i in 1L:(n_cols - 1L)) {
    grp <- cols[1L:(n_cols - i)]
    zs[, `:=`(nsum, sum(n)), by = grp]
    zs[, `:=`(ratio, n / nsum)]
    smr_i <- data.table::copy(zs)
    smr_i <- env$restore(smr_i)
    data.table::setattr(z, sprintf("summary.%d", i), smr_i)
  }
  smr <- .summary_head(z)
  data.table::setattr(z, "summary", smr)

  z <- env$restore(z)
  z <- instead::prepend_class(z, "id_data")

  z
}

# .new_id_data <- function(dt) {
#   .remove_summary_n(dt)
#   structure(
#     .Data = dt,
#     raw = NA,
#     summary = NA
#   )
# }
# m <- .new_id_data(id_data)
# attributes(m)

#' Summarise IDs with total hospital days within an underwriting window
#'
#' From an episode-level cohort (multiple rows per ID), compute **ID-level**
#' total hospital stay days (`hos`) within a window defined in months relative
#' to `uw_date`.
#'
#' This function:
#' 1. Subsets rows whose episode interval overlaps the window
#'    `[uw_date + start months, uw_date + end months)` using
#'    `to_var >= fdate & from_var < tdate`.
#' 2. Keeps only rows with `type_var == type_value` (default `"hos"`).
#' 3. Aggregates to one row per ID(+group) using [instead::count_stay()]
#'    (deduplicated stay counting).
#'
#' @param cohort A cohort data frame at **episode level** (multiple rows per ID).
#' @param id_var ID column; unquoted or character.
#' @param group_var Optional grouping column(s); unquoted or character. Use `NULL` if none.
#' @param from_var Episode start date column; unquoted or character. Must be `Date`.
#' @param to_var Episode end date column; unquoted or character. Must be `Date`.
#' @param type_var Column indicating event type (e.g., `"hos"`/`"sur"`); unquoted or character.
#' @param uw_date Underwriting date used as time zero; `"YYYY-MM-DD"`, `Date`, `POSIXt`,
#'   or a column in `cohort` (unquoted or character).
#' @param start Months **before** `uw_date` to start the window (negative numeric).
#' @param end Months **after** `uw_date` to end the window (positive numeric).
#' @param type_value Which `type_var` value to treat as hospitalisation (default `"hos"`).
#'
#' @return An object of the same base class as `cohort`, containing one row per
#'   ID(+group) with a single integer column `hos` (total hospital days in-window).
#'
#' @examples
#' \dontrun{
#' hos <- summarise_id_with_hos(
#'   cohort, id, age_band, sdate, edate, type,
#'   uw_date = "2018-07-01",
#'   start = 0, end = 36
#' )
#' head(hos)
#' }
#'
#' @seealso [summarise_id_with_sur()]
#'
#' @export
summarise_id_with_hos <- function(cohort, id_var, group_var,
                                  from_var, to_var, type_var,
                                  uw_date, start, end,
                                  type_value = "hos") {
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt  <- env$dt

  # normalize column names
  id_var    <- instead::capture_names(dt, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(dt, !!rlang::enquo(group_var))
  from_var  <- instead::capture_names(dt, !!rlang::enquo(from_var))
  to_var    <- instead::capture_names(dt, !!rlang::enquo(to_var))
  type_var  <- instead::capture_names(dt, !!rlang::enquo(type_var))
  uw_date   <- .resolve_uw_date(dt, !!rlang::enquo(uw_date))

  id_group_var <- c(id_var, group_var)

  # window subset
  dw <- .subset_by_window(dt, from_var, to_var, uw_date, start, end)

  # keep hospitalisation type only
  dw <- dw[dw[[type_var]] %chin% type_value]

  # compute stay days (deduplicated)
  if (nrow(dw)) {
    out <- instead::count_stay(
      dw,
      id_var    = id_var,
      group_var = group_var,
      from_var  = from_var,
      to_var    = to_var
    )
  } else {
    # return empty with correct columns
    out <- dt[, .SD[0], .SDcols = id_group_var]
    out[, stay := integer(0)]
  }

  # normalize to `hos`
  if ("stay" %in% names(out)) data.table::setnames(out, "stay", "hos")
  if (!"hos" %in% names(out)) stop("`count_stay()` must return column `stay`.", call. = FALSE)

  # ensure 0 for missing IDs? (usually handled by left-join upstream)
  # Here we only return IDs observed in dw; upstream merge can fill NAs with 0.

  out <- env$restore(out)
  out
}

#' Summarise IDs with total surgery count within an underwriting window
#'
#' From an episode-level cohort (multiple rows per ID), compute **ID-level**
#' total surgery count (`sur`) within a window defined in months relative
#' to `uw_date`.
#'
#' This function:
#' 1. Subsets rows whose episode interval overlaps the window
#'    `[uw_date + start months, uw_date + end months)` using
#'    `to_var >= fdate & from_var < tdate`.
#' 2. Keeps only rows with `type_var == type_value` (default `"sur"`).
#' 3. Keeps only rows with a non-missing `level_var` (and non-blank if character),
#'    because `level` indicates a surgery record in your cohort.
#' 4. Counts **one surgery per unique surgery date** per ID(+group) by de-duplicating
#'    on `from_var` (typically `sdate`).
#'
#' @param cohort A cohort data frame at **episode level** (multiple rows per ID).
#' @param id_var ID column; unquoted or character.
#' @param group_var Optional grouping column(s); unquoted or character. Use `NULL` if none.
#' @param from_var Episode start date column; unquoted or character. Must be `Date`.
#' @param to_var Episode end date column; unquoted or character. Must be `Date`.
#' @param type_var Column indicating event type (e.g., `"hos"`/`"sur"`); unquoted or character.
#' @param level_var Column indicating surgery level; unquoted or character.
#'   A row is considered surgery only if `level_var` is not missing (and not blank for character).
#' @param uw_date Underwriting date used as time zero; `"YYYY-MM-DD"`, `Date`, `POSIXt`,
#'   or a column in `cohort` (unquoted or character).
#' @param start Months **before** `uw_date` to start the window (negative numeric).
#' @param end Months **after** `uw_date` to end the window (positive numeric).
#' @param type_value Which `type_var` value to treat as surgery (default `"sur"`).
#'
#' @return An object of the same base class as `cohort`, containing one row per
#'   ID(+group) with a single integer column `sur` (surgery count in-window).
#'
#' @examples
#' \dontrun{
#' sur <- summarise_id_with_sur(
#'   cohort, id, age_band, sdate, edate, type, level,
#'   uw_date = "2018-07-01",
#'   start = 0, end = 36
#' )
#' head(sur)
#' }
#'
#' @seealso [summarise_id_with_hos()]
#'
#' @export
summarise_id_with_sur <- function(cohort, id_var, group_var,
                                  from_var, to_var, type_var, level_var,
                                  uw_date, start, end,
                                  type_value = "sur") {
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt  <- env$dt

  # normalize column names
  id_var    <- instead::capture_names(dt, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(dt, !!rlang::enquo(group_var))
  from_var  <- instead::capture_names(dt, !!rlang::enquo(from_var))
  to_var    <- instead::capture_names(dt, !!rlang::enquo(to_var))
  type_var  <- instead::capture_names(dt, !!rlang::enquo(type_var))
  level_var <- instead::capture_names(dt, !!rlang::enquo(level_var))
  uw_date   <- .resolve_uw_date(dt, !!rlang::enquo(uw_date))

  id_group_var <- c(id_var, group_var)

  # window subset
  dw <- .subset_by_window(dt, from_var, to_var, uw_date, start, end)

  # keep surgery type only
  dw <- dw[dw[[type_var]] %chin% type_value]

  # level must be non-missing; if character, must be non-blank too
  lv <- dw[[level_var]]
  if (is.character(lv)) {
    dw <- dw[!is.na(lv) & nzchar(lv)]
  } else {
    dw <- dw[!is.na(lv)]
  }

  # count one surgery per unique surgery date (from_var; typically sdate)
  if (nrow(dw)) {
    # keep only keys needed for counting (faster & avoids surprises)
    key_cols <- unique(c(id_group_var, from_var))
    dx <- unique(dw[, .SD, .SDcols = key_cols])
    out <- dx[, .(sur = as.integer(.N)), by = id_group_var]
  } else {
    # return empty with correct columns
    out <- dt[, .SD[0], .SDcols = id_group_var]
    out[, sur := integer(0)]
  }

  out <- env$restore(out)
  out
}

#' Summarise an episode-level cohort into an ID-level dataset for IR/RR/MR analyses
#'
#' From an episode-level cohort (multiple rows per ID with `[from, to]` and `kcd`),
#' build an **ID-level dataset** with 0/1 KCD flags (decl/excl/claim) around `uw_date`,
#' and optionally add ID-level count outcomes such as total hospital days (`hos`)
#' and surgery counts (`sur`) within specified windows.
#'
#' Internally converts to data.table (via [instead::ensure_dt_env()]) for speed, then
#' restores the main result and attached summaries to the original base class of
#' `cohort` (data.frame / tibble / data.table).
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
#' @param decl,decl2,decl3 Optional declaration windows as
#'   `list(start_months, end_months, pattern)`.
#' @param excl Optional exclusion window, same list form.
#' @param claim Optional claim window, same list form.
#' @param hos Optional hospitalisation window specification as
#'   `list(start_months, end_months, type_value)`. If provided, the function computes
#'   ID-level total hospital days (`hos`) and attaches `attr(x, "summary.hos")`.
#' @param sur Optional surgery window specification as
#'   `list(start_months, end_months, type_value)`. If provided, the function computes
#'   ID-level total surgery counts (`sur`) and attaches `attr(x, "summary.sur")`.
#' @param type_var Episode-type column used to identify event categories when `hos`
#'   or `sur` is provided; unquoted or character. Default is `"type"`.
#' @param level_var Episode-level column used by surgery logic when `sur` is provided;
#'   unquoted or character. Default is `"level"`.
#'
#' @details
#' The returned object is an `id_data` table (one row per ID(+group)) with:
#' - KCD flag columns (e.g. `decl*`, `excl`, `claim`) from [summarise_id_with_kcd()].
#' - Optional count outcomes `hos` and/or `sur` merged by ID(+group).
#'
#' Attached attributes (restored to the original base class):
#' - `attr(x, "summary")`: an `ir`-class (incidence rate) summary table used by
#'   RR/MR helpers.
#' - `attr(x, "summary.hos")`, `attr(x, "summary.sur")`: `mo`-class (mean outcome)
#'   summaries for `hos` / `sur` when requested.
#' - `attr(x, "raw")`: lowest-level contingency table.
#'
#' The `ir` and `mo` summaries carry metadata attributes:
#' `attr(., "decl")`, `attr(., "excl")`, `attr(., "claim")`.
#'
#' @return An object of the same base class as `cohort`, with prepended class
#'   `"id_data"`. One row per ID(+group) with KCD flags and optional outcome columns.
#'
#' @examples
#' \dontrun{
#' x <- summarise_cohort(
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
#'   claim     = list(  0, 36, "I2[0-5]|I6[0-9]|G46"),
#'   hos       = list(  0, 36, "hos"),
#'   sur       = list(  0, 36, "sur")
#' )
#' class(x)          # includes "id_data"
#' class(summary(x)) # includes "ir"
#' }
#'
#' @seealso [summarise_id_with_kcd()], [summarise_id_with_hos()],
#'   [summarise_id_with_sur()]
#'
#' @export
summarise_cohort <- function(cohort, id_var, group_var, kcd_var,
                             from_var, to_var, uw_date,
                             decl = NULL, decl2 = NULL, decl3 = NULL,
                             excl = NULL, claim = NULL,
                             hos  = NULL, sur   = NULL,
                             type_var = "type", level_var = "level") {
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt <- env$dt

  id_var    <- instead::capture_names(dt, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(dt, !!rlang::enquo(group_var))
  kcd_var   <- instead::capture_names(dt, !!rlang::enquo(kcd_var))
  from_var  <- instead::capture_names(dt, !!rlang::enquo(from_var))
  to_var    <- instead::capture_names(dt, !!rlang::enquo(to_var))
  uw_date   <- .resolve_uw_date(dt, !!rlang::enquo(uw_date))

  if (!is.null(hos) || !is.null(sur)) {
    type_var  <- instead::capture_names(dt, !!rlang::enquo(type_var))
    level_var <- instead::capture_names(dt, !!rlang::enquo(level_var))
  }

  id_group_var <- c(id_var, group_var)

  dots <- rlang::list2(
    decl  = decl,
    decl2 = decl2,
    decl3 = decl3,
    excl  = excl,
    claim = claim
  ) # rlang::list2 for `!!!` support
  dots <- instead::drop_null(dots)

  # claim
  ds <- summarise_id_with_kcd(
    cohort    = dt,
    id_var    = id_var,
    group_var = group_var,
    kcd_var   = kcd_var,
    from_var  = from_var,
    to_var    = to_var,
    uw_date   = uw_date,
    !!!dots  # `!!!` essential
  )

  # summary
  smr <- attr(ds, "summary")
  smr <- .normalize_summary_decl(smr)

  # remove summary.i
  ds <- .remove_summary_n(ds)
  dm <- data.table::copy(ds)

  # hospitalization
  if (!is.null(hos)) {
    dt_hos <- summarise_id_with_hos(
      cohort     = dt,
      id_var     = id_var,
      group_var  = group_var,
      from_var   = from_var,
      to_var     = to_var,
      type_var   = type_var,
      uw_date    = uw_date,
      start      = hos[[1L]],
      end        = hos[[2L]],
      type_value = hos[[3L]]
    )
    dm <- merge(dm, dt_hos, by = id_group_var, all.x = TRUE)
    dm[is.na(hos), hos := 0L]
  }

  # surgery
  if (!is.null(sur)) {
    dt_sur <- summarise_id_with_sur(
      cohort     = dt,
      id_var     = id_var,
      group_var  = group_var,
      from_var   = from_var,
      to_var     = to_var,
      type_var   = type_var,
      level_var  = level_var,
      uw_date    = uw_date,
      start      = sur[[1L]],
      end        = sur[[2L]],
      type_value = sur[[3L]]
    )
    dm <- merge(dm, dt_sur, by = id_group_var, all.x = TRUE)
    dm[is.na(sur), sur := 0L]
  }

  decl_list <- list(decl[[3L]], decl2[[3L]], decl3[[3L]])
  decl_list <- paste(instead::drop_null(decl_list), collapse = " & ")

  # new raw
  raw_cols  <- instead::anti_cols(ds, id_var)
  stat_cols <- c()
  if (!is.null(hos)) stat_cols <- c(stat_cols, "hos")
  if (!is.null(sur)) stat_cols <- c(stat_cols, "sur")
  if (length(stat_cols)) {
    raw <- dm[, c(
      list(n = .N),
      lapply(.SD, sum, na.rm = TRUE)
    ), by = raw_cols, .SDcols = stat_cols]
  } else {
    raw <- dm[, .(n = .N), by = raw_cols]
  }
  instead::set_col_map(raw, raw_cols, factor)
  data.table::setorderv(raw, raw_cols)

  # summary.hos / summary.sur
  smr_cols <- raw_cols[raw_cols != "claim"]
  if (!is.null(hos)) {
    smr_hos <- raw[, .(n = sum(n), hos = sum(hos, na.rm = TRUE)), by = smr_cols]
    smr_hos[, hos_mean := hos / n]
    smr_hos <- env$restore(smr_hos)
    smr_hos <- instead::prepend_class(smr_hos, "mo")
    if (!is.null(decl)) data.table::setattr(smr_hos, "decl" , decl_list)
    if (!is.null(excl)) data.table::setattr(smr_hos, "excl" , excl[[3L]])
  }
  if (!is.null(sur)) {
    smr_sur <- raw[, .(n = sum(n), sur = sum(sur, na.rm = TRUE)), by = smr_cols]
    smr_sur[, sur_mean := sur / n]
    smr_sur <- env$restore(smr_sur)
    smr_sur <- instead::prepend_class(smr_sur, "mo")
    if (!is.null(decl)) data.table::setattr(smr_sur, "decl" , decl_list)
    if (!is.null(excl)) data.table::setattr(smr_sur, "excl" , excl[[3L]])
  }

  # convert dm class
  dm <- env$restore(dm)
  dm <- instead::prepend_class(dm, "id_data")

  # add raw
  raw <- attr(dm, "raw")
  raw <- env$restore(raw)
  data.table::setattr(dm, "raw", raw)

  # summary
  smr <- env$restore(smr)
  smr <- instead::prepend_class(smr, "ir")
  if (!is.null(decl))  data.table::setattr(smr, "decl" , decl_list)
  if (!is.null(excl))  data.table::setattr(smr, "excl" , excl[[3L]])
  if (!is.null(claim)) data.table::setattr(smr, "claim", claim[[3L]])

  # add summaries
  data.table::setattr(dm, "summary", smr)
  if (!is.null(hos)) data.table::setattr(dm, "summary.hos", smr_hos)
  if (!is.null(sur)) data.table::setattr(dm, "summary.sur", smr_sur)

  dm
}

#' Summarise IDs by KCD terms to support "Mix" calculation (class-preserving)
#'
#' From an episode-level cohort (multiple rows per ID with `[from, to]` and `kcd`),
#' produce an **ID-level summary table** with 0/1 flags for each specified KCD
#' window around `uw_date`. Internally converts to data.table (via
#' [instead::ensure_dt_env()]) for speed, then restores both the main result and the
#' summaries to the original base class of `cohort` (data.frame / tibble / data.table).
#' The main result prepends S3 class `"mix_data"`, the summary prepends `"mix"`.
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
#'
#' @details
#' - Column arguments accept both unquoted and character inputs; they are
#'   normalised with `instead::capture_names()` against the working `data.table`.
#' - Flags and summaries are computed by [summarise_id_with_kcd()] and then
#'   post-processed: multiple `decl*` columns can be collapsed into a single
#'   `"decl"` factor for compactness.
#' - Attributes attached:
#'   * `attr(x, "raw")`: contingency table at the lowest level, if present (restored).
#'   * `attr(x, "summary")`: first-level summary (restored, class-prepended `"mix"`).
#'   * On the summary: `"decl"`, `"excl"` strings if provided.
#'
#' @return An object of the same base class as `cohort`, with prepended class
#'   `"id_data_for_mix"`. Contains one row per ID(+group) and 0/1 KCD flag columns.
#'   A restored summary is available at `attr(result, "summary")`
#'   (class-prepended `"mix"`).
#'
#' @examples
#' \dontrun{
#' out <- summarise_icis(
#'   cohort    = cohort,
#'   id_var    = id,
#'   group_var = age_band,
#'   kcd_var   = kcd,
#'   from_var  = sdate,
#'   to_var    = edate,
#'   uw_date   = "2018-07-01",
#'   decl      = list(-60, 0,  "I10"),
#'   decl2     = list(-60, 0,  "E78"),
#'   excl      = list(-60, 0,  "I2[0-5]|I6[0-9]|G46")
#' )
#' class(out)          # includes "id_data_for_mix"
#' class(summary(out)) # includes "mix"
#' }
#'
#' @seealso [summarise_id_with_kcd()]
#'
#' @export
summarise_icis <- function(cohort, id_var, group_var, kcd_var,
                           from_var, to_var, uw_date,
                           decl = NULL, decl2 = NULL, decl3 = NULL,
                           excl = NULL) {
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt <- env$dt

  id_var    <- instead::capture_names(dt, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(dt, !!rlang::enquo(group_var))
  kcd_var   <- instead::capture_names(dt, !!rlang::enquo(kcd_var))
  from_var  <- instead::capture_names(dt, !!rlang::enquo(from_var))
  to_var    <- instead::capture_names(dt, !!rlang::enquo(to_var))
  uw_date   <- .resolve_uw_date(dt, !!rlang::enquo(uw_date))

  id_group_var <- c(id_var, group_var)

  dots <- rlang::list2(
    decl  = decl,
    decl2 = decl2,
    decl3 = decl3,
    excl  = excl
  ) # rlang::list2 for `!!!` support
  dots <- instead::drop_null(dots)

  ds <- summarise_id_with_kcd(
    dt, id_var = id_var, group_var = group_var,
    kcd_var = kcd_var, from_var = from_var, to_var = to_var,
    uw_date = uw_date, !!!dots  # essential !!!
  )
  # summary
  smr <- .summary_tail(ds) # for mix
  smr <- .normalize_summary_decl(smr)

  decl_list <- list(decl[[3L]], decl2[[3L]], decl3[[3L]])
  decl_list <- paste(instead::drop_null(decl_list), collapse = " & ")

  # remove summary.i
  ds <- .remove_summary_n(ds)
  ds <- env$restore(ds)
  ds <- instead::prepend_class(ds, "id_data_for_mix")

  smr <- env$restore(smr)
  smr <- instead::prepend_class(smr, "mix")
  data.table::setattr(smr, "decl" , decl_list)
  data.table::setattr(smr, "excl" , excl[[3L]])
  data.table::setattr(ds, "summary", smr)

  raw <- attr(ds, "raw")
  raw <- env$restore(raw)
  data.table::setattr(ds, "raw", raw)

  ds
}

# __________________________________________ ------------------------------

# IR / Mix Summary --------------------------------------------------------

#' Summarise an `id_data` object
#'
#' @param object An object with class `"id_data"`.
#' @param ... Unused.
#'
#' @return A restored summary table stored as an attribute of `object`:
#'   `attr(object, "summary")`
#'
#' @method summary id_data
#' @export
summary.id_data <- function(object, ...) {
  attr(object, "summary")
}

#' @method summary id_data_for_mix
#' @export
summary.id_data_for_mix <- function(object, decl_level = 1, ...) {
  smr <- attr(object, "summary")
  smr <- smr[smr$decl == decl_level & smr$excl == 0]
  data.table::set(smr, j = "wt", value = smr$n / sum(smr$n))
  smr
}

# __________________________________________ ------------------------------

# Incidence Rate ----------------------------------------------------------

#' Summarise incidence rates from `id_data` objects
#'
#' Returns the incidence-rate summary table stored on an `id_data` object.
#' This is a convenience wrapper around `attr(x, "summary")` and is equivalent to
#' `summary(x)` for `id_data`.
#'
#' @param x An `id_data` object.
#' @param ... Unused.
#'
#' @return A restored incidence-rate summary table with prepended class `"ir"`,
#' stored at `attr(x, "summary")`.
#'
#' @seealso [summary.id_data()], [summarise_rr()], [summarise_mr()]
#' @export
summarise_ir <- function(x, ...) {
  UseMethod("summarise_ir")
}

#' @rdname summarise_ir
#' @export
summarise_ir.id_data <- function(x, ...) {
  instead::assert_class(x, "id_data")
  attr(x, "summary")
}

# __________________________________________ ------------------------------

# Mix Summary -------------------------------------------------------------

#' Summarise mix weights from `id_data_for_mix` objects
#'
#' Returns the mix summary table stored on an `id_data_for_mix` object and
#' computes weights (`wt`) for a given declaration level.
#'
#' This is a convenience wrapper around `attr(x, "summary")` plus filtering
#' (`decl`, `excl`) and `wt` calculation:
#' `wt = n / sum(n)` within the filtered rows.
#'
#' @param x An `id_data_for_mix` object.
#' @param decl_level Declaration level to filter on. Default is `1`.
#' @param ... Unused.
#'
#' @return A mix summary table filtered to `decl == decl_level` and `excl == 0`,
#' with an added/updated `wt` column.
#'
#' @seealso [summary.id_data_for_mix()], [summarise_ir()], [summarise_rr()], [summarise_mr()]
#' @export
summarise_mix <- function(x, decl_level = 1, ...) {
  UseMethod("summarise_mix")
}

#' @rdname summarise_mix
#' @export
summarise_mix.id_data_for_mix <- function(x, decl_level = 1, ...) {
  instead::assert_class(x, "id_data_for_mix")

  smr <- attr(x, "summary")
  smr <- smr[smr$decl == decl_level & smr$excl == 0]

  tot <- x[, .(nsum = .N), keyby = .(age_band)]
  tot[smr, `:=`(decl = i.decl, excl = i.excl, n = i.n), on = .(age_band)]
  tot[is.na(decl), decl := "1"]
  tot[is.na(excl), excl := "0"]
  tot[is.na(n)   , n    :=  0 ]
  tot[, ratio := n / nsum]
  data.table::setcolorder(tot, "nsum", after = "n")

  data.table::set(tot, j = "wt", value = tot$n / sum(tot$n))

  tot
}

# __________________________________________ ------------------------------

# Relative Risk -----------------------------------------------------------

#' Get relative risks (core engine)
#'
#' Compute RR/OR and their confidence intervals from an `id_data` object.
#' This is the internal engine used by `summarise_rr.id_data()`.
#'
#' @param x An `id_data` object. The function internally uses the incidence-rate
#'   summary table stored at `attr(x, "summary")` (class `"ir"`), which must
#'   contain at least `decl`, `excl`, `claim`, `n`, `nsum`, and `ratio`.
#' @param decl_vs Character vector of length 2 giving the declaration levels
#'   to compare (e.g., `c("0", "1")`).
#' @param conf.level Confidence level for intervals (default `0.95`).
#'
#' @details
#' The function filters to `excl == 0` and `claim == 1` on the `"ir"` summary,
#' reshapes counts into 2×2 contingency tables per stratum, then computes:
#' - RR with Wald-type confidence intervals,
#' - OR with Fisher exact test confidence intervals and p-values.
#'
#' A per-stratum list of 2×2 contingency tables (with binomial CIs by row) is
#' attached as `attr(result, "ctable")`.
#'
#' @return A restored data frame with prepended class `"rr"`. See [summarise_rr()]
#'   for output columns and attributes.
#'
#' @keywords internal
#' @export
.summarise_rr_core <- function(x, decl_vs = c("0", "1"), conf.level = .95) {
  smr <- attr(x, "summary")
  if (is.null(smr))
    stop("`x` must have `attr(x, 'summary')` (an `ir` table).", call. = FALSE)

  env <- instead::ensure_dt_env(smr)
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
  ctable <- lapply(seq_len(dim(arr)[3]), function(i) {
    m <- arr[,, i, drop = FALSE]
    dim(m) <- dim(arr)[1:2]
    dimnames(m) <- dimnames(arr)[1:2]
    m
  })
  ctable <- lapply(seq_along(ctable), function(i) {
    cbind(ctable[[i]], .binom_test(ctable[[i]][,1], ctable[[i]][,3])[, 3:5])
  })
  names(ctable) <- dimnames(arr)[[3]]

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

  data.table::setattr(dm, "decl" , attr(smr, "decl"))
  data.table::setattr(dm, "excl" , attr(smr, "excl"))
  data.table::setattr(dm, "claim", attr(smr, "claim"))
  data.table::setattr(dm, "ctable", ctable)

  dm <- instead::prepend_class(dm, "rr")
  dm <- env$restore(dm)

  dm
}

#' Summarise relative risk from `id_data` objects
#'
#' Compute relative risk (RR) and odds ratio (OR) with confidence intervals
#' from `id_data` objects by comparing two declaration levels.
#'
#' Internally, counts are reshaped into 2×2 contingency tables per stratum,
#' RR CIs are computed by a Wald method, and OR CIs and p-values by Fisher's
#' exact test. Results are returned per stratum with attributes describing the
#' declaration/exclusion/claim patterns and the contingency array.
#'
#' @param x An `id_data` object.
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
#'   - `attr(x, "ctable")`: a 2×2×K contingency array with dimnames
#'     `c("exp+","exp-")`, `c("out+","out-")`, and per-stratum labels.
#'
#' @examples
#' \dontrun{
#' # x: an 'id_data' object created upstream
#' out <- summarise_rr(x, decl_vs = c("0","1"), conf.level = 0.95)
#' head(out)
#' attr(out, "ctable")
#' }
#'
#' @export
summarise_rr <- function(x, ...) {
  UseMethod("summarise_rr")
}

#' @rdname summarise_rr
#' @export
summarise_rr.id_data <- function(x, decl_vs = c("0","1"), conf.level = .95, ...) {
  instead::assert_class(x, "id_data")
  .summarise_rr_core(x, decl_vs = decl_vs, conf.level = conf.level)
}


# __________________________________________ ------------------------------

# Mean Ratio - Hos, Sur ---------------------------------------------------

#' Get mean ratios (core engine)
#'
#' Compute mean ratios (MR) and their confidence intervals from an
#' `id_data` object for count outcomes such as total hospital days
#' (`hos`) or surgeries (`sur`).
#'
#' This is the internal engine used by `summarise_mr.id_data()`.
#' The function internally retrieves the mean-outcome summary table
#' stored at `attr(x, paste0("summary.", outcome))`
#' (class `"mo"`).
#'
#' The MR here is a **ratio of per-ID means**:
#'
#' `MR = (v1 / n1) / (v0 / n0)`,
#'
#' where:
#' - `v0`, `v1` are the summed outcome totals in the two declaration groups,
#' - `n0`, `n1` are the corresponding ID counts.
#'
#' Confidence intervals are computed on the log scale using a Wald
#' approximation:
#'
#' `se(log(MR)) ≈ sqrt(1/v1 + 1/v0)`,
#'
#' optionally with a +0.5 continuity correction when `v0` or `v1` are zero.
#'
#' @param x An `id_data` object containing mean-outcome summaries in
#'   `attr(x, "summary.hos")` and/or `attr(x, "summary.sur")`.
#' @param outcome Character scalar. The outcome column to summarise;
#'   one of `"hos"` or `"sur"`.
#' @param decl_vs Character vector of length 2 giving the declaration levels
#'   to compare (e.g., `c("0", "1")`).
#' @param conf.level Confidence level for intervals (default `0.95`).
#' @param correction Logical. If `TRUE`, applies a +0.5 continuity correction
#'   to zero totals (`v0` or `v1`) for stability of log/SE calculations.
#'
#' @return A restored data frame with prepended class `"mr"` containing
#'   per-stratum results. Columns include:
#'   - `n0`, `n1`: ID counts in reference vs. exposed groups,
#'   - `v0`, `v1`: outcome totals in reference vs. exposed groups,
#'   - `rate0`, `rate1`: per-ID means in each group,
#'   - `mr`, `mr_lower`, `mr_upper`: MR and its Wald confidence interval,
#'   - `p_value`: two-sided Wald test p-value for `H0: MR = 1`,
#'   - `decision`: factor `"reject"` / `"fail"` at level `conf.level`.
#'
#' Metadata attributes (`"decl"`, `"excl"`, `"claim"`) are propagated
#' from the underlying `"mo"` summary when present.
#'
#' @keywords internal
#' @export
.summarise_mr_core <- function(x,
                               outcome = c("hos", "sur"),
                               decl_vs = c("0", "1"),
                               conf.level = 0.95,
                               correction = TRUE) {
  outcome <- match.arg(outcome)
  which <- paste0("summary.", outcome)
  smr <- attr(x, which)
  if (is.null(smr))
    stop(sprintf("Missing attribute `%s` on `x`.", which), call. = FALSE)

  env <- instead::ensure_dt_env(smr)
  dt  <- env$dt

  instead::assert_cols(dt, c("decl", "excl", "n", outcome))

  drop_cols <- c("decl", "excl", "n", outcome, paste0(outcome, "_mean"))
  strata <- instead::anti_cols(dt, drop_cols)
  if (!length(strata)) strata <- "."

  # use only the two decl groups and non-excluded rows
  ds <- dt[excl == 0 & decl %chin% decl_vs]

  fml <- stats::as.formula(paste(paste(strata, collapse = " + "), "~ decl"))

  dn <- data.table::dcast(ds, fml, value.var = "n"    , fun.aggregate = sum)
  dv <- data.table::dcast(ds, fml, value.var = outcome, fun.aggregate = sum)

  data.table::setnames(dn, old = decl_vs, new = c("n0", "n1"))
  data.table::setnames(dv, old = decl_vs, new = c("v0", "v1"))

  out <- dv[dn, on = strata]

  out[, `:=`(
    rate0 = v0 / n0,
    rate1 = v1 / n1
  )]

  if (correction) {
    out[, `:=`(
      v0c = data.table::fifelse(v0 == 0, v0 + 0.5, v0),
      v1c = data.table::fifelse(v1 == 0, v1 + 0.5, v1)
    )]
  } else {
    out[, `:=`(v0c = v0, v1c = v1)]
  }

  out[, mr := (v1c / n1) / (v0c / n0)]

  z <- stats::qnorm((1 + conf.level) / 2)

  out[, se_log := sqrt(1 / v1c + 1 / v0c)]
  out[, mr_lower := exp(log(mr) - z * se_log)]
  out[, mr_upper := exp(log(mr) + z * se_log)]

  alpha <- 1 - conf.level

  out[, z_stat := log(mr) / se_log]
  out[, p_value := 2 * stats::pnorm(-abs(z_stat))]

  out[, decision := factor(
    data.table::fifelse(p_value < alpha, "reject", "fail"),
    levels = c("reject", "fail")
  )]

  keep <- c(
    strata, "n0", "n1", "v0", "v1", "rate0", "rate1",
    "mr", "mr_lower", "mr_upper", "p_value", "decision"
  )

  out <- out[, .SD, .SDcols = keep]

  # propagate metadata
  if (!is.null(attr(smr, "decl")))  data.table::setattr(out, "decl",  attr(smr, "decl"))
  if (!is.null(attr(smr, "excl")))  data.table::setattr(out, "excl",  attr(smr, "excl"))
  if (!is.null(attr(smr, "claim"))) data.table::setattr(out, "claim", attr(smr, "claim"))
  data.table::setattr(out, "outcome", outcome)

  out <- instead::prepend_class(out, "mr")
  out <- env$restore(out)

  out
}

#' Summarise mean ratio from `id_data` objects
#'
#' Compute mean ratios (MR) with confidence intervals from `id_data`
#' objects by comparing two declaration levels for count outcomes
#' (e.g., total hospital days `hos` or surgeries `sur`).
#'
#' Internally, totals are aggregated per stratum and declaration level,
#' then MR is computed as `(v1/n1) / (v0/n0)` with Wald-type confidence
#' intervals on the log scale.
#'
#' @param x An `id_data` object.
#' @param ... Passed to class-specific methods.
#'
#' @return A data frame (same base class as the input's restored table) with
#'   prepended class `"mr"`. Columns include:
#'   - `n0, n1`: ID counts in reference vs exposed groups,
#'   - `v0, v1`: outcome totals in reference vs exposed groups,
#'   - `rate0, rate1`: per-ID means in each group,
#'   - `mr, mr_lower, mr_upper`: MR and its Wald confidence interval,
#'   - `p_value`: two-sided Wald test p-value for `H0: MR = 1`,
#'   - `decision`: factor `"reject"` / `"fail"` based on `conf.level`.
#'
#' Attributes:
#'   - `attr(x, "decl")`, `attr(x, "excl")`, and optionally `attr(x, "claim")`.
#'
#' @examples
#' \dontrun{
#' # x: an 'id_data' object
#' mh <- summarise_mr(x, outcome = "hos")
#' ms <- summarise_mr(x, outcome = "sur", decl_vs = c("0","1"))
#' }
#'
#' @export
summarise_mr <- function(x, ...) {
  UseMethod("summarise_mr")
}

#' @rdname summarise_mr
#' @param outcome Character scalar. The count outcome column to summarise.
#'   One of `"hos"` or `"sur"`.
#' @param decl_vs Character vector of two declaration levels to compare (default `c("0","1")`).
#' @param conf.level Confidence level for intervals (default `0.95`).
#' @param correction Logical. If `TRUE`, applies a +0.5 continuity correction
#'   to zero totals for stability.
#' @export
summarise_mr.id_data <- function(x,
                                 outcome = c("hos", "sur"),
                                 decl_vs = c("0","1"),
                                 conf.level = .95,
                                 correction = TRUE,
                                 ...) {
  instead::assert_class(x, "id_data")

  .summarise_mr_core(
    x,
    outcome = outcome,
    decl_vs = decl_vs,
    conf.level = conf.level,
    correction = correction
  )
}

# __________________________________________ ------------------------------

# Plots -------------------------------------------------------------------

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
      "%s\n%.2f\n(tp=%s)",
      ifelse(x$decision == "fail", "N.S.", ""),
      x$rr, instead::as_comma(x$tp)
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

#' Mean Ratio Plot
#'
#' Draw a mean ratio (MR) plot from an `mr` object.
#'
#' @param x An `mr` object (typically the output of [summarise_mr()]).
#' @param logscale Logical; if `TRUE`, use a log scale on the y-axis.
#' @param scales Facet scales (used when faceting by `gender`):
#'   one of `"fixed"` (default), `"free"`, `"free_x"`, or `"free_y"`.
#' @param theme Theme key for [ggshort::switch_theme()]:
#'   one of `"view"` (default), `"save"`, or `"shiny"`.
#' @param ... Additional arguments passed to [ggshort::switch_theme()].
#'
#' @return A ggplot object.
#' @export
plot_mr <- function(x,
                    logscale = FALSE,
                    scales   = c("fixed", "free_y", "free_x", "free"),
                    theme    = c("view", "save", "shiny"),
                    ...) {
  instead::assert_class(x, "mr")

  scales <- match.arg(scales)
  theme  <- match.arg(theme)

  # Titles / subtitles from attributes (if present)
  decl     <- attr(x, "decl")
  excl     <- attr(x, "excl")
  claim    <- attr(x, "claim")
  outcome  <- attr(x, "outcome")

  title <- if (logscale) "Mean Ratio (log scale)" else "Mean Ratio"
  subtitle_parts <- c()
  if (!is.null(decl))    subtitle_parts <- c(subtitle_parts, sprintf("Decl: %s", decl))
  if (!is.null(excl))    subtitle_parts <- c(subtitle_parts, sprintf("Excl: %s", excl))
  if (!is.null(outcome)) subtitle_parts <- c(subtitle_parts, sprintf("Outcome: %s", toupper(outcome)))
  # if (!is.null(claim))   subtitle_parts <- c(subtitle_parts, sprintf("Claim: %s", claim))
  subtitle <- paste(subtitle_parts, collapse = " - ")

  # label: show MR and exposed total as a quick anchor (customize as you like)
  if (!instead::has_cols(x, "label")) {
    x$label <- sprintf(
      "%s\n%.2f\n(v1=%s)",
      ifelse(x$decision == "fail", "N.S.", ""),
      x$mr, instead::as_comma(x$v1)
    )
  }

  # Dynamic x-aes & facet rule (mirror plot_rr)
  has_gender   <- instead::has_cols(x, "gender")
  has_age_band <- instead::has_cols(x, "age_band")

  x_var   <- if (has_age_band) "age_band" else if (has_gender) "gender" else "."
  x_angle <- if (identical(x_var, "age_band")) 90 else 0

  ymax <- max(x$mr, na.rm = TRUE) * 1.3

  p <- ggshort::ggbar(
    data  = x,
    x     = .data[[x_var]],
    y     = .data$mr,
    ymax  = ymax,
    label = .data$label,
    label_args = list(size = 3, vjust = -.25)
  ) +
    list(if (logscale) ggplot2::scale_y_log10()) +
    list(if (has_gender && has_age_band)
      ggplot2::facet_wrap(~ gender, scales = scales)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  caption = "N.S.: Not Significant") +
    ggshort::switch_theme(theme = theme, x.angle = x_angle, y.size = 0, ...)

  p
}

#' @method plot mr
#' @export
plot.mr <- function(x,
                    logscale = FALSE,
                    scales   = c("fixed", "free_y", "free_x", "free"),
                    theme    = c("view", "save", "shiny"),
                    ...) {
  plot_mr(
    x        = x,
    logscale = logscale,
    scales   = match.arg(scales),
    theme    = match.arg(theme),
    ...
  )
}

# __________________________________________ ------------------------------

# Internal helper functions -----------------------------------------------

.subset_by_window <- function(dt, from_var, to_var, uw_date, start, end) {
  fdate <- instead::add_mon(uw_date, start)
  tdate <- instead::add_mon(uw_date, end)
  dt[(dt[[to_var]] >= fdate) & (dt[[from_var]] < tdate)]
}

.summary_head <- function(x) {
  nms <- instead::regex_attr(x, "summary\\.\\d+")
  if (!length(nms)) return(NULL)

  nums <- as.integer(sub("summary\\.", "", nms))
  attr(x, nms[which.min(nums)])
}

.summary_tail <- function(x) {
  nms <- instead::regex_attr(x, "summary\\.\\d+")
  if (!length(nms)) return(NULL)

  nums <- as.integer(sub("summary\\.", "", nms))
  attr(x, nms[which.max(nums)])
}

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

.normalize_summary_decl <- function(dt) {
  decl_cols <- instead::regex_cols(dt, "^decl")
  dt[, decl := instead::paste_cols(dt, decl_cols, sep = "+")]
  dt[, decl := as.factor(decl)]
  decl_n_cols <- instead::regex_cols(dt, "^decl[2-3]$")
  instead::rm_cols(dt, decl_n_cols)
  dt
}

.remove_summary_n <- function(dt) {
  smr_n <- instead::regex_attr(dt, "^summary\\.[0-9]+$")
  for (smr_i in smr_n) data.table::setattr(dt, smr_i, NULL)
  dt
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

.build_ctable <- function(df, group_var) {
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
  ctable <- lapply(seq_len(dim(arr)[3]), function(i) {
    m <- arr[,, i, drop = FALSE]
    dim(m) <- dim(arr)[1:2]
    dimnames(m) <- dimnames(arr)[1:2]
    m
  })
  ctable <- lapply(seq_along(ctable), function(i) {
    cbind(ctable[[i]], .binom_test(ctable[[i]][,1], ctable[[i]][,3])[, 3:5])
  })
  names(ctable) <- dimnames(arr)[[3]]
}
