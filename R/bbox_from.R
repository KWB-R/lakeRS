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
  c("north" = geosphere::destPoint(p = c(lat, lon),b = 0, d = meters)[2],
    "east" = geosphere::destPoint(p = c(lat, lon),b = 90, d = meters)[1],
    "south" = geosphere::destPoint(p = c(lat, lon),b = 180, d = meters)[2],
    "west" = geosphere::destPoint(p = c(lat, lon),b = -90, d = meters)[1]
  )
}

#' Bounding Box from rectangle
#' 
#' @param top_lat,bottom_lat Latitude of the bottom and top of the rectangle 
#' in WGS 84 EPSG:4326 (--> Top value is higher than bottom value)
#' @param left_lon,right_lon Longitude of the left and right side of the 
#' rectangle in WGS 84 EPSG:4326 (--> right side value is higher than left side
#' value)
#' @param meters The distance around the point to create the bounding box.
#' Needs to be > 0.
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
