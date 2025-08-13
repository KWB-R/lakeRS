#' Display the Bounding Box
#' 
#' @param bbox The bounding box to disply
#' @param zoom The zoom level of the initial map
#' 
#' @export
#' 
#' @importFrom leaflet leaflet addTiles setView addRectangles
#' 
display_bbox <- function(bbox, zoom = 15){
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(map = m)
  m <- leaflet::setView(
    map = m, 
    lng = mean(bbox[c(2,4)]), lat = mean(bbox[c(1,3)]), 
    zoom = zoom)
  m <- leaflet::addRectangles(
    map = m, lng1 = bbox[2], lng2 = bbox[4],
    lat1 = bbox[1], lat2 = bbox[3])
  m
}
