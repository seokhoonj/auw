#' Retrieve systolic blood pressure distribution table from KOSIS
#'
#' Downloads and processes the **systolic blood pressure** table from the
#' Korean Statistical Information Service (KOSIS).
#' Data are grouped by age, gender, blood pressure category, and survey year.
#'
#' @param age_interval Integer; width of the age band (default: 5).
#'   Passed to [add_age_band()] to group ages.
#' @param period Either `"latest"` (default, the most recent year available) or
#'   a specific survey year (numeric string, e.g. `"2020"`).
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{bp_type}{Blood pressure type, `"systolic"`}
#'     \item{age_band}{Age band factor (from `add_age_band`)}
#'     \item{gender}{Factor: M = Male, F = Female}
#'     \item{bp}{Ordered factor of blood pressure categories (e.g. `"<120"`, `">=140"`)}
#'     \item{year}{Survey year (numeric)}
#'     \item{n}{Estimated number of respondents in the cell}
#'   }
#'
#' @examples
#' \dontrun{
#' # Latest available systolic blood pressure distribution
#' sys <- get_sys_bp_table()
#' head(sys)
#'
#' # Specific year
#' sys20 <- get_sys_bp_table(period = "2023")
#' }
#'
#' @export
get_sys_bp_table <- function(age_interval = 5, period = "latest") {
  # systolic blood pressure
  sys_bp_table <- kosis::getStatData(
    orgId = "350", tblId = "DT_35007_N063",
    objL1 = "ALL", objL2 = "ALL", objL3 = "ALL"
  )
  data.table::setDT(sys_bp_table)
  instead::set_col_lower(sys_bp_table)
  if (period == "latest") {
    sys_bp_table <- sys_bp_table[prd_de == max(prd_de) & c1 != "001" & c2 != "001" & c3 != "001"]
  } else {
    sys_bp_table <- sys_bp_table[prd_de == period & c1 != "001" & c2 != "001" & c3 != "001"]
  }
  data.table::set(sys_bp_table, j = "bp_type", value = "systolic")
  data.table::setnames(
    sys_bp_table,
    c("c1_nm", "c2_nm", "c3_nm", "prd_de", "dt"),
    c("age", "gender", "bp", "year", "n")
  )
  sys_bp_table[, age := as.numeric(get_pattern(pattern = "^[0-9]+", age))]
  add_age_band(sys_bp_table, age_var = age, interval = age_interval,
               label_style = c("open"))
  sys_bp_table[, age := NULL]
  sys_bp_table[, gender := factor(ifelse(gender == "남자", "M", "F"),
                                  levels = c("M", "F"))]
  sys_bp_table[, year := as.numeric(year)]
  sys_bp_table[, n := as.numeric(n)]
  sys_bp_table[, bp := gsub("mmHg", "", bp)]
  sys_bp_table[grepl("미만", bp), bp := paste("<" , gsub(" 미만", "", bp))]
  sys_bp_table[grepl("이상", bp), bp := paste(">=", gsub(" 이상", "", bp))]
  levels <- unique(sys_bp_table$bp)
  sys_bp_table[, bp := ordered(bp, levels = levels)]

  sys_bp_table[]
}

#' Retrieve diastolic blood pressure distribution table from KOSIS
#'
#' Downloads and processes the **diastolic blood pressure** table from the
#' Korean Statistical Information Service (KOSIS).
#' Data are grouped by age, gender, blood pressure category, and survey year.
#'
#' @inheritParams get_sys_bp_table
#'
#' @return A data.table with the same structure as
#'   [get_sys_bp_table()], but with `bp_type = "diastolic"`.
#'
#' @examples
#' \dontrun{
#' # Latest available diastolic blood pressure distribution
#' dia <- get_dia_bp_table()
#' head(dia)
#'
#' # Specific year
#' dia20 <- get_dia_bp_table(period = "2023")
#' }
#'
#' @export
get_dia_bp_table <- function(age_interval = 5, period = "latest") {
  # diatolic blood pressure
  dia_bp_table <- kosis::getStatData(
    orgId = "350", tblId = "DT_35007_N061",
    objL1 = "ALL", objL2 = "ALL", objL3 = "ALL"
  )
  data.table::setDT(dia_bp_table)
  instead::set_col_lower(dia_bp_table)
  if (period == "latest") {
    dia_bp_table <- dia_bp_table[prd_de == max(prd_de) & c1 != "001" & c2 != "001" & c3 != "001"]
  } else {
    dia_bp_table <- dia_bp_table[prd_de == period & c1 != "001" & c2 != "001" & c3 != "001"]
  }
  data.table::set(dia_bp_table, j = "bp_type", value = "diastolic")
  data.table::setnames(
    dia_bp_table,
    c("c1_nm", "c2_nm", "c3_nm", "prd_de", "dt"),
    c("age", "gender", "bp", "year", "n")
  )
  dia_bp_table[, age := as.numeric(get_pattern(pattern = "^[0-9]+", age))]
  add_age_band(dia_bp_table, age_var = age, interval = age_interval,
               label_style = c("open"))
  dia_bp_table[, age := NULL]
  dia_bp_table[, gender := factor(ifelse(gender == "남자", "M", "F"),
                                  levels = c("M", "F"))]
  dia_bp_table[, year := as.numeric(year)]
  dia_bp_table[, n := as.numeric(n)]
  dia_bp_table[, bp := gsub("mmHg", "", bp)]
  dia_bp_table[grepl("미만", bp), bp := paste("<" , gsub(" 미만", "", bp))]
  dia_bp_table[grepl("이상", bp), bp := paste(">=", gsub(" 이상", "", bp))]
  levels <- unique(dia_bp_table$bp)
  dia_bp_table[, bp := ordered(bp, levels = levels)]

  dia_bp_table[]
}

