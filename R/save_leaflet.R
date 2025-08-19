#' Save a leaflet as html
#' 
#' By adding content to the html it is assured that icons are scaled according
#' to the device. In this way the map can be used by mobile devices.
#' 
#' @param x Leaflet object to be saved
#' @param file Filename including path
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