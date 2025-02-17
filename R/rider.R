#' Classify riders
#'
#' This function assigns a coverage type (`coverage_type`) and a risk type (`risk_type`)
#' to a given rider column in a dataframe based on keyword matching.
#'
#' @param df A dataframe containing rider information.
#' @param rider A string or symbol representing the column name to be categorized.
#'
#' @return The input dataframe with two new columns: `coverage_type` and `risk_type`.
#'
#' @export
classify_rider <- function(df, rider) {
  # Convert the rider column name to a string
  rider <- rlang::as_string(rlang::ensym(rider))

  # Assign the coverage type based on keywords in the rider column
  df[, coverage_type := ifelse(
    grepl("사망", get(rider)), "life", ifelse(
      grepl("정기", get(rider)), "term", ifelse(
        grepl("장해", get(rider)), "disability", ifelse(
          grepl("진단", get(rider)), "diagnosis", ifelse(
            grepl("입원", get(rider)), "hospitalization", ifelse(
              grepl("수술", get(rider)), "surgery", ifelse(
                grepl("통원", get(rider)), "outpatient", ifelse(
                  grepl("치료", get(rider)), "treatment", ifelse(
                    grepl("골절", get(rider)), "fracture", ifelse(
                      grepl("간병", get(rider)), "nursing", ifelse(
                        grepl("의료비", get(rider)), "medical_expense", ifelse(
                          grepl("장기요양", get(rider)), "long_term_care", ifelse(
                            grepl("산정특례", get(rider)), "special_calculation", ifelse(
                              grepl("납입면제", get(rider)), "premium_waiver", NA)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )]

  # Assign the risk type based on additional keywords
  df[, risk_type := ifelse(
    grepl("사망", get(rider)), "life", ifelse(
      grepl("암", get(rider)), "cancer", ifelse(
        grepl("뇌", get(rider)), "brain", ifelse(
          grepl("심", get(rider)), "heart", ifelse(
            grepl("남성질환|남성특정질환", get(rider)), "men_specific", ifelse(
              grepl("여성질환|여성특정질환", get(rider)), "women_specific", ifelse(
                grepl("치매", get(rider)), "dementia", NA)
            )
          )
        )
      )
    )
  )]
}
