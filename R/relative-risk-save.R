# __________________________________________ ------------------------------

# Save Relative Risk ------------------------------------------------------

#' Write RR report blocks into an existing openxlsx Workbook
#'
#' Writes a full RR report layout (2x2 contingency tables, RR summaries,
#' loading tables, and plots) into an **existing** `openxlsx::Workbook`.
#' This function does **not** save the workbook to disk; it returns the
#' modified workbook object.
#'
#' The layout is intended to be used by [save_rr_xlsx()], which loads/creates
#' a workbook, calls this function, then saves it.
#'
#' @param wb An `openxlsx` `Workbook`.
#' @param sheet Worksheet name to write into.
#' @param ir An object of class `"ir"`.
#' @param rr An object of class `"rr"`.
#' @param mh Optional. An object of class `"mr"` for hospitalisation.
#' @param ms Optional. An object of class `"mr"` for surgery.
#' @param mix A data.frame (or data.table) containing weights by `age_band`
#'   (must include `age_band` and `wt` columns used by internal loading tables).
#'
#' @return The updated `Workbook`.
#'
#' @export
#'
#' @seealso [save_rr_xlsx()]
save_rr_wb <- function(wb, sheet, ir, rr, mh = NULL, ms = NULL, mix) {

  instead::assert_class(ir,  "ir")
  instead::assert_class(rr,  "rr")
  if (!is.null(mh)) instead::assert_class(mh, "mr")
  if (!is.null(ms)) instead::assert_class(ms, "mr")
  instead::assert_class(mix, "data.frame")

  # ---- 1) ctable block (2x2 + RR summary) ----
  wb <- .save_ctable_block_wb(
    wb                = wb,
    sheet             = sheet,
    rr                = rr,
    rc_ctable         = c(2L, 2L),
    rc_rr             = c(7L, 2L),
    row_spacer_ctable = 3L,
    row_spacer_rr     = 6L,
    font_size         = 11,
    title_size        = 11,
    auto_width        = FALSE
  )

  # ---- 2) loading tables: RR ----
  rr_list <- .build_loading_tables(
    x               = rr,
    mix             = mix,
    metric          = "rr",
    split_by        = NULL,
    suffix_by       = "age_band",
    suffix_min_nrow = 2L
  )

  # RR loading tables at (2,7)
  wb <- .save_loading_block_wb(
    wb            = wb,
    sheet         = sheet,
    suffix_tables = rr_list$suffix_tables,
    rc            = c(2L, 7L),
    row_spacer    = 2L,

    section_title      = "Relative Risk",
    section_title_size = 14,

    data_titles     = paste0("RR / ", rr_list$suffix_ranges),
    data_title_size = 11,

    font_size  = 11,
    auto_width = FALSE,
    num_fmt    = c(
      "rr"         = "0.00",
      "rr_lower"   = "0.00",
      "rr_upper"   = "0.00",
      "p_value"    = "0.00",
      "n"          = "#,##0",
      "loading"    = "0.00",
      "wt"         = "0.00",
      "wt_loading" = "0.00"
    ),
    row_names  = FALSE
  )

  # ---- 3) loading tables: MR (HOS/SUR) ----
  mh_list <- .build_loading_tables(mh, mix, metric = "mr", suffix_min_nrow = 2L)
  ms_list <- .build_loading_tables(ms, mix, metric = "mr", suffix_min_nrow = 2L)

  if (!is.null(mr_hos_list)) {
    wb <- .save_mr_loading_block_wb(
      wb            = wb,
      sheet         = sheet,
      suffix_tables = mh_list$suffix_tables,
      suffix_ranges = mh_list$suffix_ranges,
      label         = "hos",
      rc_start      = get_next_cell_down(wb) + c(1, 0)
    )
  }

  if (!is.null(mr_sur_list)) {
    wb <- .save_mr_loading_block_wb(
      wb            = wb,
      sheet         = sheet,
      suffix_tables = ms_list$suffix_tables,
      suffix_ranges = ms_list$suffix_ranges,
      label         = "sur",
      rc_start      = get_next_cell_down(wb) + c(1, 0)
    )
  }

  # ---- 4) plots: RR / MR-HOS / MR-SUR (stacked) ----
  p_list <- list()
  if (!is.null(ir))
    p_list[["Incidence Rate"]]   <- plot_ir(ir, palette = "base")
  if (!is.null(rr))
    p_list[["Relative Risk"]]    <- plot_rr(rr, palette = "base")
  if (!is.null(mr_hos))
    p_list[["Mean Ratio - HOS"]] <- plot_mr(mr_hos, logscale = FALSE)
  if (!is.null(mr_sur))
    p_list[["Mean Ratio - SUR"]] <- plot_mr(mr_sur, logscale = FALSE)

  ggshort::suppress_geom_removed_warnings({
    wb <- instead::save_plot_wb(
      plot          = p_list,
      wb            = wb,
      sheet         = sheet,
      rc            = c(2L, 18L),
      rows_per_inch = 4.2
    )
  })

  invisible(wb)
}

#' Save RR report to an Excel file
#'
#' Creates (or loads) an Excel workbook, writes RR report blocks via
#' [save_rr_wb()], then saves the workbook to disk.
#'
#' @param ir An object of class `"ir"`.
#' @param rr An object of class `"rr"`.
#' @param mh Optional. An object of class `"mr"` for hospitalisation.
#' @param ms Optional. An object of class `"mr"` for surgery.
#' @param mix A data.frame (or data.table) containing weights by `age_band`.
#' @param file Output path. Default `"RR.xlsx"`.
#' @param sheet Worksheet name to write into. Default `"RR"`.
#' @param overwrite Logical; passed to `openxlsx::saveWorkbook()`.
#'
#' @return The file path (invisibly).
#'
#' @export
#'
#' @seealso [save_rr_wb()]
save_rr_xlsx <- function(ir, rr, mh = NULL, ms = NULL, mix,
                         file = "RR.xlsx", sheet = "RR", overwrite = FALSE) {

  wb <- if (file.exists(file)) openxlsx::loadWorkbook(file) else openxlsx::createWorkbook()
  wb <- save_rr_wb(
    wb    = wb,
    sheet = sheet,
    ir    = ir,
    rr    = rr,
    mh    = mh,
    ms    = ms,
    mix   = mix
  )

  openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)

  file
}

#' Save multiple RR plan reports to an Excel workbook
#'
#' Builds plan-specific RR report sheets from `cohort`, `icis`, and `plans`,
#' then writes:
#'
#' - one detail sheet per plan
#' - an index sheet with hyperlinks to all plan sheets
#' - summary blocks for relative risk and mean ratios on the index sheet
#'
#' For each row of `plans`, this function:
#'
#' 1. constructs plan-specific declaration / exclusion / claim rules,
#' 2. summarises cohort outcomes and ICIS mix,
#' 3. computes IR / RR / MR summaries,
#' 4. writes a detail sheet via [save_rr_wb()],
#' 5. collects long-format summary tables used to build index-sheet summaries.
#'
#' After all plans are processed, the function creates:
#'
#' - an index sheet with navigation links,
#' - wide summary tables for:
#'   - `decision`
#'   - `loading`
#'   - `n`
#'   - `nsum`
#'   - `ratio`
#'
#' These summary blocks are written side by side for:
#'
#' - Relative Risk
#' - Mean Ratio - HOS
#' - Mean Ratio - SUR
#'
#' @param cohort A data.frame containing the cohort-level input data.
#' @param icis A data.frame containing ICIS data used to build mix weights.
#' @param plans A data.frame defining one plan per row. Must contain columns
#'   used in the body of the function such as `plan`, `decl`, `excl`, and `claim`.
#' @param id_var Column in `cohort` / `icis` identifying individuals.
#' @param group_var Grouping column(s) used for summaries. Default `.(age_band)`.
#' @param kcd_var Column containing diagnosis / code information.
#' @param from_var Column containing start dates.
#' @param to_var Column containing end dates.
#' @param uw_date Underwriting date column or expression resolved by
#'   `.resolve_uw_date()`.
#' @param decl_start_end Integer length-2 vector giving the declaration window.
#'   Default `c(-60, 0)`.
#' @param excl_start_end Integer length-2 vector giving the exclusion window.
#'   Default `c(-60, 0)`.
#' @param claim_start_end Integer length-2 vector giving the claim window.
#'   Default `c(0, 36)`.
#' @param hos_start_end Integer length-2 vector giving the hospitalisation window.
#'   Default `c(0, 36)`.
#' @param sur_start_end Integer length-2 vector giving the surgery window.
#'   Default `c(0, 36)`.
#' @param add_objects Logical; if `TRUE`, writes additional reference tables to
#'   each detail sheet. Default `TRUE`.
#' @param file Output `.xlsx` file path. Default `"RR-plans.xlsx"`.
#' @param overwrite Logical; passed to [openxlsx::saveWorkbook()].
#'
#' @return Invisibly returns `file`.
#'
#' @details
#' The index sheet is written to sheet `"Index"` and contains:
#' \itemize{
#'   \item an index table with hyperlinks to each plan sheet
#'   \item summary blocks for RR / MR-HOS / MR-SUR
#' }
#'
#' Each plan sheet includes a title written at cell `B1`, followed by the
#' standard RR report layout generated by [save_rr_wb()].
#'
#' @examples
#' \dontrun{
#' save_rr_plans_xlsx(
#'   cohort = cohort,
#'   icis = icis,
#'   plans = plans,
#'   file = "RR-plans.xlsx",
#'   overwrite = TRUE
#' )
#' }
#'
#' @seealso [save_rr_wb()], [save_index_sheet_wb()]
#'
#' @export
save_rr_plans_xlsx <- function(cohort, icis, plans,
                               id_var    = id,
                               group_var = .(age_band),
                               kcd_var   = kcd,
                               from_var  = sdate,
                               to_var    = edate,
                               uw_date   = uw_date,
                               decl_start_end  = c(-60,  0),
                               excl_start_end  = c(-60,  0),
                               claim_start_end = c(  0, 36),
                               hos_start_end   = c(  0, 36),
                               sur_start_end   = c(  0, 36),
                               add_objects = TRUE,
                               file = "RR-plans.xlsx",
                               overwrite = FALSE) {
  instead::assert_class(cohort, "data.frame")
  instead::assert_class(icis  , "data.frame")
  instead::assert_class(plans , "data.frame")

  instead::check_file_overwrite(file, overwrite = TRUE)

  id_var    <- instead::capture_names(cohort, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(cohort, !!rlang::enquo(group_var))
  kcd_var   <- instead::capture_names(cohort, !!rlang::enquo(kcd_var))
  from_var  <- instead::capture_names(cohort, !!rlang::enquo(from_var))
  to_var    <- instead::capture_names(cohort, !!rlang::enquo(to_var))
  uw_date   <- .resolve_uw_date(cohort, !!rlang::enquo(uw_date))

  decl_start  <- decl_start_end[1L]
  decl_end    <- decl_start_end[2L]

  excl_start  <- excl_start_end[1L]
  excl_end    <- excl_start_end[2L]

  claim_start <- claim_start_end[1L]
  claim_end   <- claim_start_end[2L]

  hos_start   <- hos_start_end[1L]
  hos_end     <- hos_start_end[2L]

  sur_start   <- sur_start_end[1L]
  sur_end     <- sur_start_end[2L]

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Index", gridLines = FALSE, tabColour = "#FFFF00")

  instead::msg_rule("Saving RR plans workbook")

  n <- nrow(plans)

  rr_summary_list <- vector("list", n)
  mh_summary_list <- vector("list", n)
  ms_summary_list <- vector("list", n)

  for (i in seq_len(n)) {
    plan          <- plans$plan[i]
    decl_pattern  <- plans$decl[i]
    excl_pattern  <- plans$excl[i]
    claim_pattern <- plans$claim[i]

    instead::msg_step(i, n, plan)

    decl  <- list(decl_start , decl_end , decl_pattern)
    excl  <- list(excl_start , excl_end , excl_pattern)
    claim <- list(claim_start, claim_end, claim_pattern)
    hos   <- list(hos_start  , hos_end  , "hos")
    sur   <- list(sur_start  , sur_end  , "sur")

    id_data <- summarise_cohort(
      cohort    = cohort,
      id_var    = id_var,
      group_var = group_var,
      kcd_var   = kcd_var,
      from_var  = from_var,
      to_var    = to_var,
      uw_date   = uw_date,
      decl      = decl,
      excl      = excl,
      claim     = claim,
      hos       = hos,
      sur       = sur
    )

    ir <- auw::summarise_ir(id_data)
    rr <- auw::summarise_rr(id_data)
    mh <- auw::summarise_mr(id_data, outcome = "hos")
    ms <- auw::summarise_mr(id_data, outcome = "sur")

    instead::replace_nonfinite_with_na(ir)
    instead::replace_nonfinite_with_na(rr)
    instead::replace_nonfinite_with_na(mh)
    instead::replace_nonfinite_with_na(ms)

    id_data_for_mix <- auw::summarise_icis(
      cohort    = icis,
      id_var    = id_var,
      group_var = group_var,
      kcd_var   = kcd_var,
      from_var  = from_var,
      to_var    = to_var,
      uw_date   = uw_date,
      decl      = decl,
      excl      = excl
    )

    mix <- auw::summarise_mix(id_data_for_mix)

    # summary tables (long)
    rr_list <- .build_loading_tables(
      x               = rr,
      mix             = mix,
      metric          = "rr",
      split_by        = NULL,
      suffix_by       = "age_band",
      suffix_min_nrow = 2L
    )
    rr_table <- data.table::copy(rr_list$table)
    data.table::set(rr_table, j = "no"  , value = rep(i,    nrow(rr_table)))
    data.table::set(rr_table, j = "plan", value = rep(plan, nrow(rr_table)))
    data.table::setcolorder(rr_table, c("no", "plan"))
    rr_summary_list[[i]] <- rr_table

    mh_list <- .build_loading_tables(
      x               = mh,
      mix             = mix,
      metric          = "mr",
      split_by        = NULL,
      suffix_by       = "age_band",
      suffix_min_nrow = 2L
    )
    mh_table <- data.table::copy(mh_list$table)
    data.table::set(mh_table, j = "no"  , value = rep(i,    nrow(mh_table)))
    data.table::set(mh_table, j = "plan", value = rep(plan, nrow(mh_table)))
    data.table::setcolorder(mh_table, c("no", "plan"))
    mh_summary_list[[i]] <- mh_table

    ms_list <- .build_loading_tables(
      x               = ms,
      mix             = mix,
      metric          = "mr",
      split_by        = NULL,
      suffix_by       = "age_band",
      suffix_min_nrow = 2L
    )
    ms_table <- data.table::copy(ms_list$table)
    data.table::set(ms_table, j = "no"  , value = rep(i,    nrow(ms_table)))
    data.table::set(ms_table, j = "plan", value = rep(plan, nrow(ms_table)))
    data.table::setcolorder(ms_table, c("no", "plan"))
    ms_summary_list[[i]] <- ms_table

    # detail sheets
    wb <- auw::save_rr_wb(
      wb    = wb,
      sheet = plan,
      ir    = ir,
      rr    = rr,
      mh    = mh,
      ms    = ms,
      mix   = mix
    )

    wb <- instead::write_cell(
      wb        = wb,
      sheet     = plan,
      x         = plan,
      rc        = c(1, 2),
      bold      = TRUE,
      font_size = 18
    )

    if (add_objects) {
      wb <- .save_object_block_wb(
        wb     = wb,
        sheet  = plan,
        rr     = rr,
        mr_hos = mh,
        mr_sur = ms,
        mix    = mix
      )
    }
  }

  # ---- index sheet ----
  wb <- instead::save_index_sheet_wb(
    wb            = wb,
    index_sheet   = "Index",
    target_sheets = plans$plan,
    index_table   = plans,
    title         = "Index Table",
    rc_index      = c(2, 2),
    rc_target     = c(1, 1)
  )

  # ---- combine long summaries ----
  rr_summary <- data.table::rbindlist(rr_summary_list)
  mh_summary <- data.table::rbindlist(mh_summary_list)
  ms_summary <- data.table::rbindlist(ms_summary_list)

  # ---- wide summaries ----
  rr_wides <- .build_summary_wides(rr_summary)
  mh_wides <- .build_summary_wides(mh_summary)
  ms_wides <- .build_summary_wides(ms_summary)

  # ---- style rules ----
  style_rules_rr <- .make_reject_style_rules(rr_summary, rr_wides$decision)
  style_rules_mh <- .make_reject_style_rules(mh_summary, mh_wides$decision)
  style_rules_ms <- .make_reject_style_rules(ms_summary, ms_wides$decision)

  # ---- numeric formats ----
  num_fmt_decimal <- c(
    "20-29" = "0.00",
    "30-39" = "0.00",
    "40-49" = "0.00",
    "50-59" = "0.00",
    "60-69" = "0.00",
    "70-79" = "0.00"
  )

  num_fmt_comma <- c(
    "20-29" = "#,##0",
    "30-39" = "#,##0",
    "40-49" = "#,##0",
    "50-59" = "#,##0",
    "60-69" = "#,##0",
    "70-79" = "#,##0"
  )

  num_fmts <- list(
    decision = NULL,
    loading  = num_fmt_decimal,
    n        = num_fmt_comma,
    nsum     = num_fmt_comma,
    ratio    = num_fmt_decimal
  )

  # ---- start position: below index table ----
  idx_rng <- attr(wb, "instead.last_range_by_sheet")[["Index"]]
  if (is.null(idx_rng)) {
    stop("Failed to retrieve index table range for sheet 'Index'.", call. = FALSE)
  }

  start_rc <- c(
    idx_rng$start_cell[1L] - 1L,
    idx_rng$end_cell[2L] + 2L
  )

  # ---- stacked summaries ----
  res_rr <- .save_summary_wb(
    wb           = wb,
    sheet        = "Index",
    rc           = start_rc,
    summary_name = "Relative Risk",
    tables       = rr_wides,
    num_fmts     = num_fmts,
    style_rules  = style_rules_rr,
    row_spacer   = 3L
  )
  wb <- res_rr$wb

  res_mh <- .save_summary_wb(
    wb           = wb,
    sheet        = "Index",
    rc           = res_rr$next_rc,
    summary_name = "Mean Ratio - HOS",
    tables       = mh_wides,
    num_fmts     = num_fmts,
    style_rules  = style_rules_mh,
    row_spacer   = 3L
  )
  wb <- res_mh$wb

  res_ms <- .save_summary_wb(
    wb           = wb,
    sheet        = "Index",
    rc           = res_mh$next_rc,
    summary_name = "Mean Ratio - SUR",
    tables       = ms_wides,
    num_fmts     = num_fmts,
    style_rules  = style_rules_ms,
    row_spacer   = 3L
  )
  wb <- res_ms$wb

  openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)

  instead::msg_rule("", line = 1)
  instead::msg_done("Workbook saved:", file)
  instead::msg_rule("", line = 1)

  invisible(file)
}

# __________________________________________ ------------------------------

# Internal helper functions -----------------------------------------------

# 1. save_rr_wb -----------------------------------------------------------

## 1) write layers --------------------------------------------------------

#' Save 2x2 contingency tables and RR summaries to a workbook (internal)
#'
#' Writes:
#' 1) A section title ("2x2 Contingency Table")
#' 2) 2x2 tables from `attr(rr, "ctable")` (first 3 columns only)
#' 3) RR summary tables (`rr`, `rr_lower`, `rr_upper`, `p_value`)
#'
#' Note: This helper does **not** write the "Relative Risk" section title.
#' That title is handled by the loading-block writer.
#'
#' @param wb An `openxlsx` `Workbook`.
#' @param sheet Sheet name.
#' @param rr An object of class `"rr"`.
#' @param rc_ctable Start cell (`c(row, col)`) for the ctable section title.
#'   The ctable tables are written starting one row below this cell.
#' @param rc_rr Start cell (`c(row, col)`) for the RR tables start position.
#'   The RR summary tables are written starting one row below this cell.
#' @param row_spacer_ctable Row spacing between ctable tables.
#' @param row_spacer_rr Row spacing between RR summary tables.
#' @param font_size Font size for table bodies.
#' @param title_size Font size for per-table titles written by `save_data_wb()`.
#' @param section_title_size Font size for the ctable section title.
#' @param auto_width Logical; passed to [instead::save_data_wb()].
#'
#' @return The updated workbook.
#' @keywords internal
.save_ctable_block_wb <- function(wb, sheet, rr,
                                  rc_ctable = c(3L, 2L),
                                  rc_rr     = c(8L, 2L),
                                  row_spacer_ctable = 3L,
                                  row_spacer_rr     = 6L,
                                  font_size  = 11,
                                  title_size = 11,
                                  section_title_size = 14,
                                  auto_width = FALSE) {
  instead::assert_class(rr, "rr")

  if (!is.character(sheet) || length(sheet) != 1L || is.na(sheet))
    stop("`sheet` must be a single sheet name.", call. = FALSE)

  # ensure sheet exists (write_cell assumes it)
  if (!(sheet %in% openxlsx::sheets(wb))) {
    openxlsx::addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  }

  # --- ctable section title ---
  wb <- instead::write_cell(
    wb, sheet,
    x = "2x2 Contingency Table",
    rc = rc_ctable,
    bold = TRUE,
    font_name = getOption("instead.font"),
    font_size = section_title_size,
    h_align = "left",
    v_align = "center"
  )

  # --- 2x2 contingency tables (start one row below section title) ---
  ctable <- attr(rr, "ctable")
  if (is.null(ctable))
    stop("`rr` must have attr(rr, 'ctable').", call. = FALSE)

  ctable <- lapply(
    ctable,
    function(m) m[, c("outcome+", "outcome-", "sum"), drop = FALSE]
  )

  wb <- instead::save_data_wb(
    data       = ctable,
    wb         = wb,
    sheet      = sheet,
    rc         = c(rc_ctable[1L] + 1L, rc_ctable[2L]),
    row_spacer = row_spacer_ctable,
    row_names  = TRUE,
    font_size  = font_size,
    title_size = title_size,
    auto_width = auto_width,
    num_fmt = c(
      "outcome+" = "#,##0",
      "outcome-" = "#,##0",
      "sum"      = "#,##0"
    )
  )

  # --- RR summary tables (start one row below section title) ---
  res <- rr[, .(rr, lower = rr_lower, upper = rr_upper, p_value)]
  res <- unname(split(res, seq_len(nrow(res))))

  wb <- instead::save_data_wb(
    data       = res,
    wb         = wb,
    sheet      = sheet,
    rc         = c(rc_rr[1L] + 1L, rc_rr[2L]),
    row_spacer = row_spacer_rr,
    row_names  = FALSE,
    font_size  = font_size,
    title_size = title_size,
    auto_width = auto_width,
    num_fmt = c(
      "rr"      = "0.00",
      "lower"   = "0.00",
      "upper"   = "0.00",
      "p_value" = "0.00"
    )
  )

  invisible(wb)
}

#' Save a loading-table block to a workbook (internal)
#'
#' Writes a block consisting of:
#' - An optional section title (e.g., "Relative Risk", "Mean Ratio - HOS")
#' - One or more loading tables stacked vertically (via `save_data_wb()`)
#' - A bold, underlined total under the `wt_loading` column for each table
#'
#' Requires `save_data_wb()` to record table ranges in `attr(wb, "instead.ranges")`.
#'
#' @param wb An `openxlsx` `Workbook`.
#' @param sheet Sheet name.
#' @param suffix_tables A list of loading tables (typically suffix tables)
#'   written vertically to the worksheet.
#' @param rc Start cell (`c(row, col)`) for the block. If `section_title` is set,
#'   the title is written at `rc` and tables start one row below.
#' @param row_spacer Blank rows between stacked tables.
#' @param section_title Optional section heading written above the block.
#' @param section_title_size Font size for `section_title`.
#' @param data_titles Optional character vector of per-table titles passed to
#'   `save_data_wb()`.
#' @param data_title_size Font size for per-table titles in `save_data_wb()`.
#' @param font_size Font size for table bodies.
#' @param auto_width Logical; passed to `save_data_wb()`.
#' @param num_fmt Numeric format passed to `save_data_wb()`.
#' @param row_names Logical; passed to `save_data_wb()`.
#'
#' @return The updated workbook.
#' @keywords internal
.save_loading_block_wb <- function(wb, sheet, suffix_tables,
                                   rc = c(3L, 7L),
                                   row_spacer = 2L,
                                   section_title = NULL,
                                   section_title_size = 14,
                                   data_titles = NULL,
                                   data_title_size = 11,
                                   font_size = 11,
                                   auto_width = FALSE,
                                   num_fmt = "0.00",
                                   row_names = FALSE) {

  # section title (optional)
  if (!is.null(section_title)) {
    wb <- instead::write_cell(
      wb = wb,
      sheet = sheet,
      x = section_title,
      rc = rc,
      bold  = TRUE,
      font_name = getOption("instead.font"),
      font_size = section_title_size,
      h_align = "left",
      v_align = "center"
    )
    rc <- c(rc[1L] + 1L, rc[2L]) # tables start one row below the section title
  }

  # write loading tables (stacked)
  wb <- instead::save_data_wb(
    data        = suffix_tables,
    wb          = wb,
    sheet       = sheet,
    rc          = list(rc),
    row_spacer  = row_spacer,
    data_titles = data_titles,
    title_size  = data_title_size,
    font_size   = font_size,
    num_fmt     = num_fmt,
    auto_width  = auto_width,
    row_names   = row_names
  )

  # retrieve the ranges of the tables just written
  ranges <- attr(wb, "instead.ranges")
  if (is.null(ranges) || length(ranges) < length(suffix_tables)) {
    stop(
      "No recorded table ranges found in workbook. ",
      "`save_data_wb()` must record ranges.",
      call. = FALSE
    )
  }
  ranges <- utils::tail(ranges, length(suffix_tables))

  # find the column index of wt_loading
  wt_idx <- match("wt_loading", names(suffix_tables[[1L]]))
  if (is.na(wt_idx))
    stop("Column 'wt_loading' not found.", call. = FALSE)

  # row names offset
  rn_off <- if (row_names) 1L else 0L

  # write total values under each table
  for (i in seq_along(suffix_tables)) {
    rng <- ranges[[i]]
    tot <- sum(suffix_tables[[i]]$wt_loading, na.rm = TRUE)

    wb <- instead::write_cell(
      wb = wb,
      sheet = sheet,
      x = round(tot, 2),
      rc = c(rng$end_cell[1L] + 1L, rng$start_cell[2L] + rn_off + wt_idx - 1L),
      bold = TRUE,
      underline = TRUE,
      font_name = getOption("instead.font"),
      font_size = font_size,
      h_align = "right",
      v_align = "center"
    )
  }

  invisible(wb)
}

.save_mr_loading_block_wb <- function(wb, sheet,
                                      suffix_tables,
                                      suffix_ranges,
                                      label,
                                      rc_start,
                                      row_spacer = 2L,
                                      section_title_size = 14,
                                      data_title_size = 11,
                                      font_size = 11,
                                      auto_width = FALSE,
                                      num_fmt = "0.00",
                                      row_names = FALSE) {

  if (is.null(suffix_tables)) return(wb)

  wb <- .save_loading_block_wb(
    wb = wb,
    sheet = sheet,
    suffix_tables = suffix_tables,
    rc = rc_start,
    row_spacer = row_spacer,

    section_title      = paste0("Mean Ratio - ", toupper(label)),
    section_title_size = section_title_size,

    data_titles     = suffix_ranges,
    data_title_size = data_title_size,

    font_size  = font_size,
    auto_width = auto_width,
    num_fmt    = num_fmt,
    row_names  = row_names
  )

  invisible(wb)
}

.save_object_block_wb <- function(wb, sheet, rr, mr_hos = NULL, mr_sur = NULL,
                                  mix, rc = c(2, 28)) {
  wb <- instead::write_cell(
    wb, sheet,
    x = "Reference",
    rc = rc,
    bold = TRUE,
    font_name = getOption("instead.font"),
    font_size = 14,
    h_align = "left",
    v_align = "center"
  )

  data_list <- list()
  data_list[["Relative Risk"]] <- rr
  if (!is.null(mr_hos)) data_list[["Mean Ratio - HOS"]] <- mr_hos
  if (!is.null(mr_sur)) data_list[["Mean Ratio - SUR"]] <- mr_sur
  if (!is.null(mix)) data_list[["ICIS Mix"]] <- mix

  data_titles <- names(data_list)

  wb <- instead::save_data_wb(
    data        = data_list,
    wb          = wb,
    sheet       = sheet,
    rc          = rc + c(1, 0),
    data_titles = data_titles,
    title_size  = 11,
    auto_width  = FALSE,
    num_fmt = c(
      tp = "#,##0",	fn = "#,##0",	fp = "#,##0",	tn = "#,##0",
      n0 = "#,##0", n1 = "#,##0", v0 = "#,##0", v1 = "#,##0",
      inc0 = "0.00", inc1 = "0.00",
      rr = "0.00", rr_lower = "0.00", rr_upper = "0.00",
      or = "0.00", or_lower = "0.00", or_upper = "0.00",
      rate0 = "0.00", rate1 = "0.00",
      mr = "0.00", mr_lower = "0.00", mr_upper = "0.00",
      p_value = "0.00",
      decl = "0", excl = "0"
    )
  )
}

## 2) data layers ---------------------------------------------------------

#' Build loading tables from summary results
#'
#' Builds loading tables by combining a summary table `x` with a mix table
#' `mix`, then computing:
#'
#' - `loading = metric - 1`
#' - `wt_loading = loading * wt`
#'
#' If `suffix_by` is supplied, suffix tables are generated using
#' [instead::split_suffix_tables()], and weights are re-normalised within
#' each suffix table.
#'
#' @param x A summary table such as an RR or MR result.
#' @param mix A mix table containing weight information (must include
#'   columns such as `n`, `nsum`, `ratio`, and `wt` used for weighting).
#' @param metric Character scalar giving the metric column name
#'   (e.g., `"rr"` or `"mr"`). Default `"rr"`.
#' @param split_by Optional character vector of grouping columns used before
#'   suffix tables are generated.
#' @param suffix_by Optional character vector of ordered block columns used to
#'   generate suffix tables. Default `"age_band"`.
#' @param suffix_min_nrow Integer; minimum number of rows required for a
#'   generated suffix table to be kept. Default `2L`.
#'
#' @return A list with four components:
#' \describe{
#'   \item{table}{The base loading table.}
#'   \item{suffix_tables}{A list of suffix tables generated from the base table.}
#'   \item{suffix_ranges}{Character vector describing the suffix ranges
#'   corresponding to each table.}
#'   \item{suffix_totals}{Numeric vector of weighted loading totals
#'   for each suffix table.}
#' }
#'
#' @examples
#' \dontrun{
#' rr_list <- build_loading_tables(rr, mix, metric = "rr")
#' mr_list <- build_loading_tables(mr, mix, metric = "mr")
#' }
#'
#' @keywords internal
.build_loading_tables <- function(x, mix,
                                  metric    = c("rr", "mr"),
                                  split_by  = NULL,
                                  suffix_by = "age_band",
                                  suffix_min_nrow = 2L) {
  metric <- match.arg(metric)
  lower  <- paste0(metric, "_lower")
  upper  <- paste0(metric, "_upper")

  tbl <- data.table::copy(data.table::as.data.table(x))
  mix <- data.table::copy(data.table::as.data.table(mix))

  # join keys are the common columns between x and mix
  on_cols <- instead::intersect_cols(tbl, mix)

  # keep only the columns needed for loading tables
  cols <- c(on_cols, metric, lower, upper, "p_value", "decision")

  tbl <- tbl[, cols, with = FALSE]

  # merge weights from mix using all common keys
  tbl[mix, `:=`(n = i.n, nsum = i.nsum, ratio = i.ratio, wt = i.wt), on = on_cols]
  tbl[is.na(tbl$n)    , n     := 0]
  tbl[is.na(tbl$nsum) , nsum  := 0]
  tbl[is.na(tbl$ratio), ratio := 0]

  # compute base loading columns
  tbl <- .add_loadings(dt = tbl, metric = metric, renorm_wt = FALSE)

  # default split variables:
  # common keys excluding suffix_by; if none remain, treat as a single group
  if (is.null(split_by)) {
    split_by <- if (is.null(suffix_by)) on_cols else setdiff(on_cols, suffix_by)
    if (!length(split_by)) split_by <- NULL
  }

  # build suffix tables
  tbl_list <- if (is.null(suffix_by)) {
    list(tbl[, -c("nsum", "ratio"), with = FALSE])
  } else {
    instead::split_suffix_tables(
      x = tbl[, -c("nsum", "ratio"), with = FALSE],
      by = suffix_by,
      split_by = split_by,
      min_nrow = suffix_min_nrow
    )
  }

  # renormalise wt within each suffix table and recompute loading columns
  tbl_list <- lapply(tbl_list, function(x) {
    .add_loadings(
      dt = data.table::copy(x),
      metric = metric,
      renorm_wt = TRUE
    )
  })

  # build suffix range labels
  range_col <- if ("age_band" %chin% names(tbl)) {
    "age_band"
  } else if (!is.null(suffix_by)) {
    suffix_by[1L]
  } else {
    NULL
  }

  suffix_ranges <- vapply(
    tbl_list,
    function(x) {
      if (!is.null(range_col) && range_col %chin% names(x)) {
        get_age_band_range(x[!is.na(x$loading),][[range_col]])
      } else {
        ""
      }
    },
    character(1L)
  )

  # weighted loading totals
  suffix_totals <- vapply(
    tbl_list,
    function(x) sum(x[["wt_loading"]], na.rm = TRUE),
    numeric(1L)
  )

  suffix_nrows <- vapply(
    tbl_list,
    function(x) nrow(x),
    numeric(1L)
  )

  suffix_rejects <- vapply(
    tbl_list,
    function(x) sum(x$decision == "reject", na.rm = TRUE),
    numeric(1L)
  )

  suffix_fails <- vapply(
    tbl_list,
    function(x) sum(x$decision == "fail", na.rm = TRUE),
    numeric(1L)
  )

  list(
    table = tbl,
    suffix_tables  = tbl_list,
    suffix_ranges  = suffix_ranges,
    suffix_totals  = suffix_totals,
    suffix_nrows   = suffix_nrows,
    suffix_rejects = suffix_rejects,
    suffix_fails   = suffix_fails
  )
}

.add_loadings <- function(dt, metric, renorm_wt = FALSE) {
  data.table::setDT(dt)

  loading <- dt[[metric]] - 1

  if (renorm_wt) {
    ok <- is.finite(loading)

    wt <- numeric(nrow(dt))
    s  <- sum(dt$wt[ok], na.rm = TRUE)

    if (is.finite(s) && s > 0) {
      wt[ok] <- dt$wt[ok] / s
    } else {
      wt[] <- NA_real_
    }

    data.table::set(dt, j = "wt", value = wt)
  }

  data.table::set(dt, j = "loading", value = loading)
  data.table::set(dt, j = "wt_loading", value = loading * dt$wt)

  cols <- c("loading", "wt", "wt_loading")
  data.table::setcolorder(dt, c(instead::anti_cols(dt, cols), cols))

  dt
}

# 2. save_rr_plans_xlsx ---------------------------------------------------

#' @keywords internal
#' @noRd
.save_summary_wb <- function(wb, sheet, rc, summary_name,
                             tables, num_fmts, style_rules,
                             row_spacer = 3L,
                             col_spacer = 2L) {

  current_rc  <- rc
  max_end_col <- rc[2L]

  for (nm in names(tables)) {

    wb <- instead::save_data_wb(
      data        = tables[[nm]],
      wb          = wb,
      sheet       = sheet,
      rc          = current_rc,
      data_titles = paste0(summary_name, " - ", nm),
      num_fmt     = num_fmts[[nm]],
      style_rules = style_rules
    )

    rng <- attr(wb, "instead.last_range")

    max_end_col <- max(max_end_col, rng$end_cell[2L])

    current_rc <- c(
      rng$end_cell[1L] + row_spacer,
      rc[2L]
    )
  }

  list(
    wb = wb,
    next_rc = c(rc[1L], max_end_col + col_spacer)
  )
}

#' @keywords internal
#' @noRd
.build_summary_wides <- function(summary, fml = "no + plan ~ age_band") {

  out <- list(
    decision = data.table::dcast(summary, fml, value.var = "decision"),
    loading  = data.table::dcast(summary, fml, value.var = "loading"),
    n        = data.table::dcast(summary, fml, value.var = "n"),
    nsum     = data.table::dcast(summary, fml, value.var = "nsum"),
    ratio    = data.table::dcast(summary, fml, value.var = "ratio")
  )

  for (nm in names(out)) {
    instead::replace_nonfinite_with_na(out[[nm]])
  }

  out
}

#' @keywords internal
#' @noRd
.make_reject_style_rules <- function(summary, reject_tbl) {

  list(
    list(
      cols = as.character(sort(unique(summary$age_band))),
      data = reject_tbl,
      condition = function(x) x == "reject",
      style = instead::create_style(
        fg_fill = "#FFFF00",
        bold = TRUE
      )
    )
  )
}

