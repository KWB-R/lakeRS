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