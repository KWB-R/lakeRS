#' Display pixel locations on an interactive map
#'
#' Projects input coordinates to WGS 84 and displays them as circle markers on a
#' leaflet map.
#'
#' @param x Numeric vector of x coordinates (e.g., easting or longitude in the
#'   source coordinate reference system).
#' @param y Numeric vector of y coordinates (e.g., northing or latitude in the
#'   source coordinate reference system). Must be the same length as \code{x}.
#' @param crs Character string specifying the source coordinate reference
#'   system of \code{x} and \code{y}, in a format understood by
#'   \code{terra::project()} (e.g., \code{"EPSG:32633"}).
#' @param labels Optional character vector of labels for the markers. If
#'   provided, its length should match the length of \code{x} and \code{y}.
#'
#' @details The marker colors are currently hard-coded as \code{c("blue", "green",
#'   "orange", "purple")}. If the number of points exceeds the number of colors,
#'   colors will be recycled.
#'
#' @return A `leaflet` map object.
#'
#' @export
#' 
show_pixels <- function(
    x,
    y,
    crs,
    labels = NULL
){

  p_wgs84 <- terra::project(
    x = matrix(data = c(x, y), ncol = 2, byrow = FALSE),
    from = crs,
    to = "EPSG:4326")
  
    m <- leaflet::leaflet()
    m <- leaflet::setView(
      map = m, 
      lng = mean(p_wgs84[,1]), 
      lat = mean(p_wgs84[,2]), 
      zoom = 13
    )
    m <- leaflet::addProviderTiles(map = m, provider = "OpenStreetMap.Mapnik")
    
    leaflet::addCircleMarkers(
      map = m, 
      lng = p_wgs84[,1], 
      lat = p_wgs84[,2], 
      label = labels, 
      color = c("blue", "green", "orange", "purple"))
}