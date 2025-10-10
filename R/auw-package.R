#' @description
#' This package is developed for actuarial underwriting.
#' @keywords internal
#' @importFrom ggplot2 aes coord_flip element_rect geom_hline geom_label
#'   facet_wrap labs scale_color_gradientn scale_fill_gradient
#'   scale_x_continuous scale_y_log10 theme xlab ylab
#' @importFrom ggshort add_title geom_hline1 get_legend ggbar ggline ggmix
#'   grob_to_ggplot hstack_plots hstack_plots_with_legend scale_color_gender
#'   scale_fill_gender scale_fill_pair_manual scale_y_comma stat_mean_hline
#'   switch_theme vstack_plots
#' @importFrom data.table `:=` `.SD` `%chin%` address copy frank melt set
#'   setcolorder setnames setorder setorderv uniqueN
#' @importFrom grDevices rainbow
#' @importFrom instead add_mon assert_class capture_names del_pattern draw_line
#'   ensure_dt_env get_pattern has_cols has_ptr match_cols paste_list paste_str
#'   paste_uni_str replace_na_with_empty replace_na_with_zero
#'   replace_cols_in_mat rm_cols rotate seq_list set_band set_dt set_tibble
#'   unilen zeros
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @importFrom rlang .data as_name enquo enquos ensym ensyms has_length list2 syms
#' @importFrom scales comma
#' @importFrom stats binom.test fisher.test formula median pnorm qnorm
#' @importFrom stringr str_pad
#' @importFrom utils globalVariables head tail
"_PACKAGE"
