
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

check_ae_ratio <- function(df) {
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
