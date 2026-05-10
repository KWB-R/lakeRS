#' Create a bounding box around a point
#'
#' Calculates the north, east, south, and west limits of a square-like bounding
#' box around a point in WGS 84 coordinates. The limits are computed by moving a
#' fixed geodesic distance from the point in the four cardinal directions.
#'
#' @param lat Numeric scalar. Latitude of the center point in WGS 84
#'   (`EPSG:4326`).
#' @param lon Numeric scalar. Longitude of the center point in WGS 84
#'   (`EPSG:4326`).
#' @param meters Numeric scalar greater than zero. Distance in meters from the
#'   point to each side of the bounding box. Default is `20`.
#'
#' @return A named numeric vector with elements `north`, `east`, `south`, and
#'   `west`, suitable for [start_openEO_job()] or [display_geometry()].
#'
#' @importFrom geosphere destPoint
#' @export
#' 
bbox_from_point <- function(lat, lon, meters = 20){
  c("north" = geosphere::destPoint(p = c(lon, lat), b = 0, d = meters)[2],
    "east" = geosphere::destPoint(p = c(lon, lat), b = 90, d = meters)[1],
    "south" = geosphere::destPoint(p = c(lon, lat), b = 180, d = meters)[2],
    "west" = geosphere::destPoint(p = c(lon, lat), b = -90, d = meters)[1]
  )
}

#' Create a buffered bounding box around a rectangle
#'
#' Expands a latitude-longitude rectangle by a geodesic buffer distance and
#' returns the resulting north, east, south, and west limits.
#'
#' @param top_lat Numeric scalar. Northern latitude of the rectangle in WGS 84
#'   (`EPSG:4326`). Must be greater than `bottom_lat`.
#' @param bottom_lat Numeric scalar. Southern latitude of the rectangle in WGS 84
#'   (`EPSG:4326`).
#' @param left_lon Numeric scalar. Western longitude of the rectangle in WGS 84
#'   (`EPSG:4326`). Must be smaller than `right_lon`.
#' @param right_lon Numeric scalar. Eastern longitude of the rectangle in WGS 84
#'   (`EPSG:4326`).
#' @param meters Numeric scalar greater than zero. Buffer distance in meters.
#'   Default is `20`.
#'
#' @return A named numeric vector with elements `north`, `east`, `south`, and
#'   `west`.
#'
#' @importFrom geosphere destPoint
#' @export
bbox_from_rectangle <- function(
    top_lat, bottom_lat, left_lon, right_lon, meters = 20
){
  c(
    "north" = geosphere::destPoint(
      p = c(right_lon, top_lat),b = 0, d = meters)[2], 
    "east" = geosphere::destPoint(
      p = c(right_lon, top_lat),b = 90, d = meters)[1],
    "south" = geosphere::destPoint(
      p = c(right_lon, bottom_lat),b = 180, d = meters)[2],
    "west" = geosphere::destPoint(
      p = c(left_lon, top_lat),b = -90, d = meters)[1]
  )
}