#' Classify rider coverage
#'
#' This function classifies a given rider column in a dataframe into
#' a general coverage type (`coverage_type`) and a specific risk type (`risk_type`).
#'
#' If multiple categories match, they are combined as a comma-separated string.
#'
#' @param df A dataframe containing rider information.
#' @param rider A string or symbol representing the column name to be classified.
#'
#' @return The input dataframe with two new columns: `coverage_type` and `risk_type`.
#'
#' @export
classify_rider <- function(df, rider) {
  # Convert the rider column name to a string
  rider <- rlang::as_string(rlang::ensym(rider))

  # Define coverage type mapping
  coverage_keywords <- list(
    "life"                = "사망",
    "term"                = "정기",
    "disability"          = "장해",
    "diagnosis"           = "진단",
    "hospitalization"     = "입원",
    "surgery"             = "수술",
    "outpatient"          = "통원",
    "treatment"           = "치료",
    "fracture"            = "골절",
    "nursing"             = "간병",
    "emergency_room"      = "응급실",
    "medical_expense"     = "의료비|검사비",
    "long_term_care"      = "장기요양",
    "special_calculation" = "산정특례",
    "premium_waiver"      = "납입면제"
  )

  # Define risk type mapping
  risk_keywords <- list(
    "life"           = "사망",
    "cancer"         = "암",
    "brain"          = "뇌",
    "heart"          = "심근|심장|심질환|심혈",
    "liver"          = "간",
    "lung"           = "폐",
    "kidney"         = "신장",
    "tuberculosis"   = "결핵",
    "men_specific"   = "남성질환|남성특정질환",
    "women_specific" = "여성질환|여성특정질환",
    "dementia"       = "치매"
  )

  # Vectorized classification function
  classify_vector <- function(text_vector, keyword_list) {
    # 각 텍스트에 대한 빈 리스트 생성
    result_list <- vector("list", length(text_vector))

    for (category_name in names(keyword_list)) {
      matches <- grepl(keyword_list[[category_name]], text_vector)
      # 매칭되는 위치에 카테고리 추가
      result_list[matches] <- lapply(result_list[matches], c, category_name)
    }

    # 리스트를 쉼표로 구분된 문자열로 변환
    result <- sapply(result_list, paste, collapse = ",")
    result[result == ""] <- NA_character_
    return(result)
  }

  # Apply vectorized classification
  df[, `:=`(
    coverage_type = classify_vector(.SD[[rider]], coverage_keywords),
    risk_type = classify_vector(.SD[[rider]], risk_keywords)
  )]

  return(df)
}
