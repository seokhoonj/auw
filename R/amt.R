#' Face amount mix plot
#'
#' Draw a face amount mix plot.
#'
#' @param amt_mix a data.frame specifying a face amount mix by gender, age_band
#' @param label_size a string specifying label size
#' @param theme a string specifying a [ggshort::switch_theme()] function ("view", "save", "shiny")
#' @return a gtable object
#'
#' @export
amt_plot <- function(amt_mix,
                     label_args = list(size = 4),
                     theme = c("view", "save", "shiny")) {
# to be updated -----------------------------------------------------------

  theme <- match.arg(theme)

  instead::has_cols(
    amt_mix, c("gender", "age_band", "amt", "n", "nsum", "prop"),
    error_raise = TRUE
  )
  age_band <- amt <- gender <- label <- nsum <- prop <- NULL
  amt_mix_uni <- unique(amt_mix[, .(gender, age_band, nsum)])
  amt_mix_uni[, label := instead::as_comma(nsum)]
  ymax <- max(amt_mix_uni$n * 1.1)
  width <- nchar(max(amt_mix$amt))

  g1 <- ggbar(amt_mix_uni, x = age_band, y = nsum, ymax = ymax * 1.1,
              group = gender, fill = gender, label = label,
              label_args = list(unlist(label_args), hjust = .1)) +
    scale_fill_gender() +
    scale_y_comma() +
    coord_flip() +
    facet_wrap(~ gender, ncol = 1) + xlab("age band") +
    ggshort::switch_theme(theme = theme, x.size = 0, legend.position = "none")
  g2 <- ggmix(amt_mix, x = age_band, y = prop, group = amt,
              fill = amt, label = label, label_args = label_args) +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", labels = function(x)
      stringr::str_pad(format(x, big.mark = ",", scientific = F), width = width)) +
    coord_flip() + facet_wrap(~ gender, ncol = 1) + xlab("") + # theme_test() +
    ggshort::switch_theme(x.size = 0, x.angle = 90, legend.position = "right")
  legend <- get_legend(g2)
  g2 <- g2 + ggshort::switch_theme(theme = theme, x.size = 0, legend.position = "none")
  p <- ggshort::hstack_plots_with_legend(g1, g2, legend)

  grob_to_ggplot(p)
}

# sample_amt_by_insured <- function(amt_info, insured,
#                          rider_var = c("rider_name"),
#                          cond_var = c("age_band"),
#                          seed = 123) {
#   data.table::setDT(amt_info)
#   data.table::setDT(insured)
#
#   vars <- c(rider_var, cond_var)
#   amt_info_sum <- amt_info[, .(n = sum(n)), keyby = c(vars, "amt")]
#   amt_info_sum[, prob := n / sum(n), keyby = vars]
#   riders <- unique(amt_info_sum[[rider_var]])
#
#   # insured에 행 인덱스 추가
#   insured[, .idx := .I]
#
#   set.seed(seed)
#   for (rider in riders) {
#     cat("Processing rider:", rider, "\n")
#
#     # 해당 특약 데이터
#     rider_data <- amt_info_sum[get(rider_var) == rider]
#
#     # data.table join을 사용한 벡터화된 매칭
#     # insured와 rider_data를 조건 변수들로 join
#     merged <- rider_data[insured, on = cond_var, nomatch = NA, allow.cartesian = TRUE]
#
#     if (nrow(merged) > 0) {
#       # 각 insured 행에 대해 amt를 확률적으로 샘플링
#       merged[!is.na(amt), sampled_amt := {
#         if (.N == 1) {
#           amt[1]  # 선택지가 하나뿐이면 그것을 선택
#         } else {
#           sample(amt, size = 1, prob = prob)  # 여러 선택지가 있으면 확률적 샘플링
#         }
#       }, by = .idx]
#
#       # 결과를 insured에 할당
#       result_map <- merged[!is.na(sampled_amt), .(sampled_amt = sampled_amt[1]), by = .idx]
#       insured[result_map$`.idx`, (rider) := result_map$sampled_amt]
#     }
#   }
#
#   # 임시 컬럼 제거
#   insured[, .idx := NULL]
#
#   return(insured)
# }
