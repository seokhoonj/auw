% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alr.R
\name{alr_uym_plot}
\alias{alr_uym_plot}
\title{Actual loss ratio by each UY months}
\usage{
alr_uym_plot(
  x,
  group_var,
  period_var = "uym",
  elapsed_var = "elpm",
  scales = c("fixed", "free_y", "free_x", "free"),
  theme = c("view", "save", "shiny"),
  ...
)
}
\arguments{
\item{x}{an alr.data object}

\item{group_var}{a name of the group variable}

\item{period_var}{a name of the period variable ("uym", "uy")}

\item{elapsed_var}{a name of the elapsed variable ("elpm", "elp")}

\item{scales}{Should \code{scales} be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?}

\item{theme}{a string specifying a ggshort theme function ("view", "save", "shiny")}

\item{...}{ggshort theme arguments}
}
\value{
a ggplot object
}
\description{
Draw an actual loss ratio by each UY months.
}
