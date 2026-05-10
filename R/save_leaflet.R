#' Save a leaflet map as a self-contained HTML file
#'
#' Adds a mobile-friendly viewport meta tag to a leaflet widget and saves it as a
#' self-contained HTML file.
#'
#' @param x A `leaflet`  object.
#' @param file Character scalar. Output HTML file including path.
#'
#' @return The return value of [htmlwidgets::saveWidget()]. The main side effect
#'   is writing `file`.
#'
#' @importFrom htmlwidgets prependContent saveWidget
#' @importFrom htmltools tags
#' @export
#' 
save_leaflet <- function(x, file){
  x <- htmlwidgets::prependContent(
    x,
    htmltools::tags$head(
      htmltools::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
      )
    )
  )
  
  htmlwidgets::saveWidget(
    widget = x, 
    file = file, 
    selfcontained = TRUE
  )
}