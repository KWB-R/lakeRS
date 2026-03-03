#' Display pixel locations on an interactive map
#'
#' This function projects planar coordinates to WGS84 (EPSG:4326) and displays
#' the corresponding pixel locations as circle markers on an interactive
#' leaflet map centered on the mean coordinate of all points.
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
#' @details
#' The input coordinates are combined into a two-column matrix and reprojected
#' from the given \code{crs} to WGS84 (EPSG:4326). An interactive leaflet map
#' is then initialized, centered on the mean longitude and latitude of the
#' projected points, and OpenStreetMap tiles are added as a basemap.
#' Finally, the function adds circle markers at each projected coordinate,
#' optionally with labels.
#'
#' The marker colors are currently hard-coded as \code{c("blue", "green",
#' "orange", "purple")}. If the number of points exceeds the number of colors,
#' colors will be recycled.
#'
#' @return
#' An object of class \code{leaflet} representing the interactive map with
#' the pixel locations. The map is also rendered in interactive contexts
#' (e.g., RStudio Viewer, web browser).
#'
#' @examples
#' \dontrun{
#' # Example coordinates in UTM zone 33N (EPSG:32633)
#' x <- c(389425, 389500, 389575, 389650)
#' y <- c(5741395, 5741450, 5741505, 5741560)
#' crs <- "EPSG:32633"
#' labels <- paste("Pixel", seq_along(x))
#'
#' show_pixels(x = x, y = y, crs = crs, labels = labels)
#' }
#'
#' @export
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