#' Bounding Box from point coordinate
#' 
#' @param lon,lat Longitude and latitude in WGS 84 EPSG:4326 
#' @param meters The distance around the point to create the bounding box.
#' Needs to be > 0.
#' 
#' @export
#' 
#' @importFrom geosphere destPoint
#' 
bbox_from_point <- function(lon, lat, meters = 20){
  c("north" = geosphere::destPoint(p = c(lat, lon), b = 0, d = meters)[2],
    "east" = geosphere::destPoint(p = c(lat, lon), b = 90, d = meters)[1],
    "south" = geosphere::destPoint(p = c(lat, lon), b = 180, d = meters)[2],
    "west" = geosphere::destPoint(p = c(lat, lon), b = -90, d = meters)[1]
  )
}

#' Bounding Box from rectangle
#' 
#' @param top_lat,bottom_lat Latitude of the bottom and top of the rectangle 
#' in WGS 84 EPSG:4326 (--> Top value is higher than bottom value)
#' @param left_lon,right_lon Longitude of the left and right side of the 
#' rectangle in WGS 84 EPSG:4326 (--> right side value is higher than left side
#' value)
#' @param meters The buffer distance in m around the rectangle to create the 
#' bounding box (Needs to be > 0)
#' 
#' @export
#' 
#' @importFrom geosphere destPoint
#' 
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


#' Bounding Box from rectangle
#' 
#' @param geom_sfc A simple feature polygon in WGS 84 EPSG:4326. 
#' See example how to create a sfc_POLYGON from Well-known-text (wkt) Geometry.
#' @param meters The buffer distance in m around the polygon to create the 
#' bounding box (Needs to be > 0)
#' 
#' @examples
#' library(sf)
#' x <- data.frame(
#'  "wkt" = paste0(
#'    "POLYGON ((13.22103 52.61554, 13.22112 52.61558,13.22118 52.6156,",
#'    " 13.2212 52.61561, 13.22123 52.61561, 13.22127 52.61562))")
#' )
#' x$wkt
#' is(x$wkt)
#' geom_sfc <- sf::st_as_sf(x = x, wkt = "wkt", crs = 4326)
#' geom_sfc
#' is(geom_sfc)
#' 
#' @export
#' 
#' 
bbox_from_polygon <- function(
    geom_sfc, meters = 20
){
  bb <- attributes(geom_sfc)$bbox

  bbox_from_rectangle(
    top_lat = bb[["ymax"]], 
    bottom_lat = bb[["ymin"]],
    left_lon = bb[["xmin"]], 
    right_lon = bb[["xmax"]], 
    meters = meters)
}
