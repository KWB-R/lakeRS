#' Plots water scene proportions of each pixel in bounding box
#' 
#' @param ncLayer This is a layer that corresponds to the x and y dimensions of
#' the netCDF file. It can be a band of netCDF or further processed layer
#' created by [waterscene_proportion()]
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param aboveValues Lower limits of value classes. If NULL (default) 
#' numerical values will be used.
#' @param highestValue If Null the highest value is derived by the maximum of
#' all available layer values. It can also be specified manually.
#' @param aboveColors A vector of colors corresponding to the value
#' classes. 
#' @param valueRange Minimum and maximum values used for color scale of numeric 
#' values.
#' @param zoom The initial zoom level of the map
#' @param legendTitle Character string for legend title
#' @param plotLegend If True legend will be plotted
#' 
#' @importFrom raster raster projectExtent projectRaster values
#' @importFrom leaflet colorFactor colorNumeric leaflet setView addTiles addRasterImage addLegend
#' 
#' @export
#' 
plot_layer <- function(
    ncLayer, 
    nc,
    aboveValues = NULL,
    highestValue = NULL,
    aboveColors = NULL,
    valueRange = NULL,
    zoom = 15,
    legendTitle = NULL,
    plotLegend = TRUE
){
  if(all(is.na(ncLayer))){
    stop("No available data for this layer -> All values are NA")
  }
 
  hexValues <- FALSE
  if(is.character(ncLayer)){
    if(all(grepl(pattern = "^#", ncLayer))){
      hexValues <- TRUE
      COLOR <- ncLayer
      ncLayer <- matrix(data = 1:length(ncLayer), nrow = nrow(ncLayer), ncol = ncol(ncLayer))
    }
  }
  
  r <- raster::raster(
    x = ncLayer, 
    xmn = min(nc$x),
    xmx = max(nc$x),
    ymn = min(nc$y),
    ymx = max(nc$y),
    crs = raster::crs(nc$crs))
  
  to_wgs84 <- raster::projectExtent(
    object = r,
    crs = "EPSG:4326"
  )
  r_wgs84 <- raster::projectRaster(
    from = r, to = to_wgs84,
    method = "ngb"
  )

  if(hexValues){
    r_factor <- as.factor(raster::values(r_wgs84))
    raster::values(r_wgs84) <- r_factor
    pal <- leaflet::colorFactor(palette = as.vector(COLOR), 
                                domain = seq_along(levels(r_factor)), 
                                na.color = "#00000000")
    
    m <- leaflet::leaflet()
    m <- leaflet::setView(
      map = m, 
      lng = mean(raster::extent(r_wgs84)[1:2]), 
      lat = mean(raster::extent(r_wgs84)[3:4]), 
      zoom = zoom
    )
    m <- leaflet::addProviderTiles(map = m, provider = "OpenStreetMap.Mapnik")
    #m <- leaflet::addTiles(map = m, layerId = "Esri.WorldTopoMap")
    m <- leaflet::addRasterImage(
      map = m, 
      x = r_wgs84, 
      colors = pal, 
      opacity = 0.8
    )
  } else {
    if(!is.null(aboveValues)){
      if(is.null(highestValue)){
        highestValue <- max(raster::values(r_wgs84), na.rm = TRUE)
      }
      cuts <- c(aboveValues, highestValue) #set breaks
      r_factor <- cut(
        x = raster::values(r_wgs84), 
        breaks = cuts, 
        right = TRUE, 
        include.lowest = TRUE
      )
      raster::values(r_wgs84) <- r_factor
      pal <- leaflet::colorFactor(palette = aboveColors, 
                                  domain = seq_along(levels(r_factor)), 
                                  na.color = "#00000000")
    } else {
      if(is.null(valueRange)){
        valueRange <- range(raster::values(r_wgs84))
        valueRange <- valueRange + (diff(valueRange) * c(-0.00001, 0.00001))
      }
      pal <- leaflet::colorNumeric(
        palette = c("white", "black"), 
        domain = valueRange)
    }
    
    m <- leaflet::leaflet()
    m <- leaflet::setView(
      map = m, 
      lng = mean(raster::extent(r_wgs84)[1:2]), 
      lat = mean(raster::extent(r_wgs84)[3:4]), 
      zoom = zoom
    )
    m <- leaflet::addProviderTiles(map = m, provider = "OpenStreetMap.Mapnik")
    #m <- leaflet::addTiles(map = m, layerId = "Esri.WorldTopoMap")
    m <- leaflet::addRasterImage(
      map = m, 
      x = r_wgs84, 
      colors = pal, 
      opacity = 0.8
    )  
    if(plotLegend){
      if(!is.null(aboveValues)){
        legend_i <- seq_along(aboveValues)
        if(length(aboveValues) > 10){
          legend_i <- round(seq(1, length(aboveValues), length.out = 10))
        }
        m <- leaflet::addLegend(
          map = m, 
          #pal = pal,
          colors = aboveColors[legend_i], 
          labels = levels(r_factor)[legend_i],
          title = legendTitle)
      } else {
        m <- leaflet::addLegend(
          map = m, 
          pal = pal, 
          values = valueRange,
          title = legendTitle)
      }
      
    }
    
  }
  m
}
