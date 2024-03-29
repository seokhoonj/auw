% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alr.R
\name{alr_mean_plot}
\alias{alr_mean_plot}
\title{Mean of actual cumulative loss ratio}
\usage{
alr_mean_plot(
  x,
  group_var,
  elapsed_var = "elpm",
  color_type = c("base", "deep"),
  scales = c("fixed", "free_y", "free_x", "free"),
  theme = c("view", "save", "shiny"),
  ...
)
}
\arguments{
\item{x}{an alr.data object}

\item{group_var}{a name of the group variable}

\item{elapsed_var}{a name of the elapsed variable ("elpm", "elp")}

\item{color_type}{a string of color type, base and deep}

\item{scales}{Should \code{scales} be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?}

\item{theme}{a string specifying a ggshort theme function ("view", "save", "shiny")}

\item{...}{ggshort theme arguments}
}
\value{
a ggplot object
}
\description{
Draw mean of actual cumulative loss ratio
}
