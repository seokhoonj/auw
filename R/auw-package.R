#' @description
#' Provides tools for disease cohort analysis and A/E (Actual-to-Expected)
#' ratio simulation, integrating both actuarial and underwriting perspectives.
#' Designed to support morbidity experience studies, evaluate underwriting
#' bias, and visualize portfolio-level risk outcomes.
#'
#' This package is developed primarily for actuarial underwriting workflows.
#'
#' @keywords internal
#'
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
#' @importFrom instead add_mon assert_class capture_names check_col_spec
#'   check_file_overwrite check_file_recreate check_sheet_names create_style
#'   del_pattern draw_line drop_null ensure_dt_env get_next_cell_down
#'   get_next_cell_right get_pattern has_cols has_ptr intersect_cols longer
#'   match_cols msg_rule msg_step msg_done paste_cols paste_str paste_uni_str
#'   reorder_sheets replace_na_with_empty replace_na_with_zero
#'   replace_cols_in_mat rm_cols rotate save_data_wb save_plot_wb seq_list
#'   set_band set_col_map set_dt set_tibble unilen write_cell write_formula
#'   zeros
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @importFrom rlang .data as_name enquo enquos ensym ensyms has_length list2 syms
#' @importFrom stats binom.test fisher.test formula median pnorm qnorm
#' @importFrom utils globalVariables head tail
"_PACKAGE"
