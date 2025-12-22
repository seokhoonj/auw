#' Summarise population counts by group
#'
#' Aggregates unique individual counts across specified grouping variables,
#' such as gender or age bands, and computes relative proportions.
#'
#' @description
#' This function summarises a cohort dataset into a population-level view.
#' It counts the number of unique individuals within each group (using `uniqueN()`)
#' and returns both raw counts (`n`) and proportions (`prop`).
#'
#' @param cohort A `data.frame` or `data.table` containing cohort data.
#' @param id_var Column specifying unique identifiers for individuals.
#'   Supports non-standard evaluation (unquoted names or character strings).
#' @param group_var Column(s) used for grouping (e.g., `gender`, `age_band`).
#'   Supports NSE or character vectors.
#'
#' @return A `population` object (data.frame-like) with the following columns:
#' \itemize{
#'   \item `group_var` — grouping columns (e.g., `gender`, `age_band`)
#'   \item `n` — unique count of individuals
#'   \item `prop` — proportion of total population
#' }
#'
#' @details
#' Internally, the data is converted to `data.table` for efficiency and
#' restored to the original class upon return. The resulting object gains
#' an additional class `"population"`, allowing use with [plot.population()].
#'
#' @examples
#' \donttest{
#' cohort <- data.frame(
#'   id = sample(1:1000, 1000, TRUE),
#'   gender = factor(sample(c("M", "F"), 1000, TRUE)),
#'   age_band = factor(sample(c("20-29", "30-39", "40-49"), 1000, TRUE),
#'                     ordered = TRUE)
#' )
#' pop <- summarise_population(cohort)
#' head(pop)
#' }
#'
#' @seealso [plot_population()]
#'
#' @export
summarise_population <- function(cohort, id_var = "id",
                                 group_var = c("gender", "age_band")) {
  instead::assert_class(cohort, "data.frame")

  env <- instead::ensure_dt_env(cohort)
  dt  <- env$dt

  id_var    <- instead::capture_names(dt, !!rlang::enquo(id_var))
  group_var <- instead::capture_names(dt, !!rlang::enquo(group_var))

  population <- dt[, .(n = data.table::uniqueN(.SD)), keyby = group_var,
                   .SDcols = id_var]

  data.table::set(population, j = "prop", value = population$n / sum(population$n))

  population <- env$restore(population)
  instead::prepend_class(population, "population")
}

#' Plot population distribution
#'
#' Creates a bar chart showing the population distribution by age band
#' and (optionally) gender. Designed for objects created by
#' [summarise_population()].
#'
#' @param x A `population` object returned by [summarise_population()].
#' @param show_label Logical; if `TRUE` (default), display count labels
#'   above each bar.
#' @param label_args A named list of arguments controlling label appearance,
#'   passed to [ggshort::ggbar()]. Common options include:
#'   `family`, `size`, `angle`, `hjust`, `vjust`, `color`.
#' @param theme One of `"view"`, `"save"`, or `"shiny"`. Controls the
#'   ggplot2 theme applied via [ggshort::switch_theme()].
#' @param ... Additional arguments forwarded to [ggshort::switch_theme()].
#'
#' @return A `ggplot` object visualising population counts by group.
#'
#' @details
#' - If a `gender` column is present, bars are filled by gender using
#'   [ggshort::scale_fill_gender()].
#' - The y-axis is formatted with [ggshort::scale_y_comma()] for readability.
#' - Label positioning and style can be customized via `label_args`.
#'
#' @examples
#' \dontrun{
#' cohort <- data.frame(
#'   id = sample(1:1000, 1000, TRUE),
#'   gender = factor(sample(c("M", "F"), 1000, TRUE)),
#'   age_band = factor(sample(c("20-29", "30-39", "40-49"), 1000, TRUE),
#'                     ordered = TRUE)
#' )
#'
#' pop <- summarise_population(cohort)
#' plot_population(pop)
#' plot_population(pop, show_label = FALSE, theme = "save")
#' }
#'
#' @seealso [summarise_population()]
#'
#' @export
plot_population <- function(x, show_label = TRUE,
                            label_args = list(
                              family = getOption("ggshort.font"),
                              size = 4, angle = 90,
                              hjust = -.1, vjust = -.5,
                              color = "black"
                            ),
                            theme = c("view", "save", "shiny"), ...) {
  instead::assert_class(x, "population")
  theme <- match.arg(theme)

  has_gender <- instead::has_cols(x, "gender")

  if (has_gender) {
    if (show_label) {
      p <- ggshort::ggbar(x, x = .data$age_band, y = .data$n,
                          ymax = max(.data$n) * 1.3, fill = .data$gender,
                          label = instead::as_comma(n), label_args = label_args) +
        ggshort::scale_fill_gender() +
        ggshort::scale_y_comma() +
        ggshort::switch_theme(theme = theme, ...)
    } else {
      p <- ggshort::ggbar(x, x = .data$age_band, y = .data$n,
                          ymax = max(.data$n) * 1.3, fill = .data$gender) +
        ggshort::scale_fill_gender() +
        ggshort::scale_y_comma() +
        ggshort::switch_theme(theme = theme, ...)
    }
  } else {
    if (show_label) {
      p <- ggshort::ggbar(x, x = .data$age_band, y = .data$n,
                          ymax = max(.data$n) * 1.3,
                          label = instead::as_comma(n), label_args = label_args) +
        ggshort::scale_y_comma() +
        ggshort::switch_theme(theme = theme, ...)
    } else {
      p <- ggshort::ggbar(x, x = .data$age_band, y = .data$n,
                          ymax = max(.data$n) * 1.3) +
        ggshort::scale_y_comma() +
        ggshort::switch_theme(theme = theme, ...)
    }
  }

  p
}

#' Plot method for population objects
#'
#' S3 method for [plot()] that delegates to [plot_population()] for objects
#' of class `population`.
#'
#' @inheritParams plot_population
#' @param ... Additional arguments forwarded to [plot_population()].
#'
#' @return A ggplot object created by [plot_population()].
#'
#' @seealso [plot_population()], [summarise_population()]
#'
#' @examples
#' \dontrun{
#' cohort <- data.frame(
#'   id = sample(1:1000, 1000, TRUE),
#'   gender = factor(sample(c("M", "F"), 1000, TRUE)),
#'   age_band = factor(sample(c("20-29", "30-39", "40-49"), 1000, TRUE),
#'                     ordered = TRUE)
#' )
#' pop <- summarise_population(cohort)
#' plot(pop)  # equivalent to plot_population(pop)
#' }
#'
#' @method plot population
#' @export
plot.population <- function(x, show_label = TRUE,
                            label_args = list(
                              family = getOption("ggshort.font"),
                              size = 4, angle = 90,
                              hjust = -.1, vjust = .5,
                              color = "black"
                            ),
                            theme = c("view", "save", "shiny"), ...) {
  plot_population(x, show_label, label_args, theme, ...)
}
