
ggshort_theme <- function(theme = c("view", "save", "shiny"), ...) {
  theme <- match.arg(theme)
  switch(theme,
         view  = ggshort::theme_view(...),
         save  = ggshort::theme_save(...),
         shiny = ggshort::theme_shiny(...))
}
