#' Display the Bounding Box
#' 
#' @param geom The geometry to display. Either a bounding box or a polygon
#' @param zoom The zoom level of the initial map
#' 
#' @details
#' Bounding Box as a names vector or 
#' c("north" = lat1, "east"  = lon1, "south" = lat2, "west" = lon2) or a list of
#' pairs list(c(lat1, lng2), c(lat2, lon2), c(lat3, lon3))
#' 
#' @export
#' 
#' @importFrom leaflet leaflet addTiles setView addRectangles
#' 
display_geometry <- function(geom, zoom = 15){
 
  view_location <- if(is.numeric(geom)){
    c(mean(geom[c(2,4)]),
      mean(geom[c(1,3)])
    )
  } else if (is.list(geom)){
    geom <- lapply(geom, function(x){x[2:1]})
    c(mean(sapply(geom, function(x){x[1]})), 
      mean(sapply(geom, function(x){x[2]}))
    )
  }
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(map = m)
  m <- leaflet::setView(
    map = m, 
    lng = view_location[1], lat = view_location[2], 
    zoom = zoom)
  
  if(is.numeric(geom)){
    m <- leaflet::addRectangles(
      map = m, lng1 = geom[2], lng2 = geom[4],
      lat1 = geom[1], lat2 = geom[3])
  }else if (is.list(geom)){
    m <- leaflet::addPolygons(
      map = m, 
      lng = sapply(geom, function(x){x[1]}), 
      lat = sapply(geom, function(x){x[2]})
    )
  }
  m
}
