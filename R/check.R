#' Validate cohort data structure
#'
#' Ensures that a cohort dataset contains all required columns with
#' expected types and formats. Intended for validating input data prior
#' to risk analysis or incidence rate computation.
#'
#' @param df A data.frame representing a cohort data.
#'
#' @details
#' Required columns and their expected types:
#' \itemize{
#'   \item `id` (`character`): Unique subject identifier
#'   \item `gender` (`factor`): Gender category
#'   \item `age` (`integer`): Age in years
#'   \item `age_band` (`ordered`): Age-band classification
#'   \item `grade` (`integer`): Policy grade or health level
#'   \item `kcd` (`character`): KCD diagnosis code
#'   \item `type` (`character`): Record type (e.g., exposure, claim)
#'   \item `sdate`, `edate` (`Date`): Start and end dates of observation
#'   \item `level` (`integer`): Severity or category level
#' }
#'
#' @return Invisibly returns `TRUE` if validation succeeds; otherwise throws an error.
#'
#' @seealso [instead::check_col_spec()]
#'
#' @export
check_cohort <- function(df) {
  instead::assert_class(df, "data.frame")
  col_spec <- list(
    id       = "character",
    gender   = "factor",
    age      = "integer",
    age_band = "ordered",
    grade    = "integer",
    kcd      = "character",
    type     = "character",
    sdate    = "Date",
    edate    = "Date",
    level    = "integer"
  )
  instead::check_col_spec(df, col_spec)
}

#' Validate risk-rate reference table
#'
#' Checks that a risk book (reference table for base risk rates)
#' has all required columns with correct data types.
#'
#' @param df A data.frame representing a risk book.
#'
#' @details
#' Expected columns:
#' \itemize{
#'   \item `risk_cd` (`character`): Risk code
#'   \item `risk_nm` (`character`): Risk name or description
#'   \item `gender` (`factor`): Gender category
#'   \item `age` (`integer`): Age
#'   \item `rate` (`numeric`): Base risk rate
#' }
#'
#' @return Invisibly returns `TRUE` if validation passes.
#'
#' @seealso [instead::check_col_spec()]
#'
#' @export
check_risk_book <- function(df) {
  instead::assert_class(df, "data.frame")
  col_spec <- list(
    risk_cd = "character",
    risk_nm = "character",
    gender = "factor",
    age = "integer",
    rate = "numeric"
  )
  instead::check_col_spec(df, col_spec)
}

#' Validate claim reference table
#'
#' Ensures that a claim book contains all necessary columns for
#' claim ratio, disease category, and waiting/reduction rules.
#'
#' @param df A data.frame representing a claim book.
#'
#' @details
#' Expected columns:
#' \itemize{
#'   \item `rn`: Record index (`numeric`)
#'   \item `main_cat_nm`, `sub_cat_nm`: Main and sub disease categories (`character`)
#'   \item `rd_cat_cd`, `rd_cat_nm`: Risk-disease category code and name (`character`)
#'   \item `rd_cd`, `rd_nm`: Risk-disease code and name (`character`)
#'   \item `risk_cd`, `risk_nm`: Risk item code and name (`character`)
#'   \item `ratio_cd`, `ratio_nm`: Ratio code and name (`character`)
#'   \item `cvd_kcd`, `cvd_level`: Cardiovascular-related codes/levels (`character`)
#'   \item `rp_times`, `claim_times`: Reporting and claim counts (`numeric`)
#'   \item `one_time`: Binary flag for one-time claims (`integer`)
#'   \item `expiration`: Expiration period (`numeric`)
#'   \item `waiting_start`, `waiting_end`, `waiting_ratio`: Waiting period details (`numeric`)
#'   \item `reduction_start`, `reduction_end`, `reduction_ratio`: Reduction period details (`numeric`)
#' }
#'
#' @return Invisibly returns `TRUE` if validation passes.
#'
#' @seealso [instead::check_col_spec()]
#'
#' @export
check_claim_book <- function(df) {
  instead::assert_class(df, "data.frame")
  col_spec <- list(
    rn              = "numeric",
    main_cat_nm     = "character",
    sub_cat_nm      = "character",
    rd_cat_cd       = "character",
    rd_cat_nm       = "character",
    rd_cd           = "character",
    rd_nm           = "character",
    risk_cd         = "character",
    risk_nm         = "character",
    ratio_cd        = "character",
    ratio_nm        = "character",
    cvd_kcd         = "character",
    cvd_level       = "character",
    rp_times        = "numeric",
    claim_times     = "numeric",
    one_time        = "integer",
    expiration      = "numeric",
    waiting_start   = "numeric",
    waiting_end     = "numeric",
    waiting_ratio   = "numeric",
    reduction_start = "numeric",
    reduction_end   = "numeric",
    reduction_ratio = "numeric"
  )
  instead::check_col_spec(df, col_spec)
}

#' Validate A/E ratio dataset
#'
#' Ensures that an A/E (Actual-to-Expected) dataset contains all columns
#' required for exposure, loss, and claim ratio calculations.
#'
#' @param df A data.frame representing A/E ratio data.
#'
#' @details
#' Expected columns:
#' \itemize{
#'   \item `cy`, `cym`: Calendar year/month of claim (`numeric`, `Date`)
#'   \item `uy`, `uym`: Underwriting year/month (`numeric`, `Date`)
#'   \item `elp`, `elpm`: Exposure (years/months) (`numeric`)
#'   \item `pd_cat_cd`, `pd_cat_nm`, `pd_cd`, `pd_nm`: Product codes/names (`character`)
#'   \item `cv_cat_cd`, `cv_cat_nm`, `cv_cd`, `cv_nm`: Coverage codes/names (`character`)
#'   \item `rd_cat_cd`, `rd_cat_nm`, `rd_cd`, `rd_nm`: Risk-disease codes/names (`character`)
#'   \item `age_band`: Age band (`ordered`)
#'   \item `gender`: Gender (`factor`)
#'   \item `chn_cd`, `chn_nm`: Channel code and name (`character`)
#'   \item `n_sales`: Number of policies sold (`numeric`)
#'   \item `rp`: Reported claims (`numeric`)
#'   \item `loss`: Incurred loss amount (`numeric`)
#' }
#'
#' @return Invisibly returns `TRUE` if validation passes.
#'
#' @seealso [instead::check_col_spec()]
#'
#' @export
check_aer <- function(df) {
  instead::assert_class(df, "data.frame")
  col_spec <- list(
    cy        = "numeric",
    cym       = "Date",
    uy        = "numeric",
    uym       = "Date",
    elp       = "numeric",
    elpm      = "numeric",
    pd_cat_cd = "character",
    pd_cat_nm = "character",
    pd_cd     = "character",
    pd_nm     = "character",
    cv_cat_cd = "character",
    cv_cat_nm = "character",
    cv_cd     = "character",
    cv_nm     = "character",
    rd_cat_cd = "character",
    rd_cat_nm = "character",
    rd_cd     = "character",
    rd_nm     = "character",
    age_band  = "ordered",
    gender    = "factor",
    chn_cd    = "character",
    chn_nm    = "character",
    n_sales   = "numeric",
    rp        = "numeric",
    loss      = "numeric"
  )
  instead::check_col_spec(df, col_spec)
}
