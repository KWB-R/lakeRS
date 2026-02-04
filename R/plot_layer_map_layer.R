#' Leaflet Map one layer (Matrix) described by a netCDF
#' 
#' The plot is the layer on top of a leaflet map. The color scheme can
#' be done for numeric or categorical values.
#' 
#' @param ncLayer This is a layer that corresponds to the x and y dimensions of
#' the netCDF file. It can be a band of netCDF or further processed layer
#' created by [waterscene_proportion()]
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param aboveValues Lower limits of value classes. If NULL (default) 
#' numerical values will be used.
#' @param highestValue If Null the highest value is derived by the maximum of
#' all available layer values. It can also be specified manually.
#' @param aboveColors A vector of R-colors names corresponding to the value
#' classes. 
#' @param valueRange Minimum and maximum values used for color scale of numeric 
#' values.
#' @param legendTitle Character string for legend title
#' @param plotLegend If TRUE legend will be plotted. If now aboveValues and no
#' valueRange are spefecied. No legend will be plotted.
#' 
#' @details
#' Use case: Proportions of a scene.
#' The above values and correspoding colors need to be specified. If for example 
#' values between 0 and 0.5 (50%) should bewhite, values between 0.5 and 0.75 
#' should be yellow and values above 0.75 should be purple, aboveValues are 
#' c(0, 0.5, 0.75) and the aboveColors arec("white", "yellow", "purple"). 
#' In that case a legendTitle is recommended to explain what is displayed.
#' 
#' 
#' 
#' @importFrom raster raster projectExtent projectRaster values
#' @importFrom leaflet colorFactor colorNumeric leaflet setView addTiles addRasterImage addLegend
#' 
#' @export
#' 
map_layer <- function(
    ncLayer, 
    nc,
    aboveValues = NULL,
    highestValue = NULL,
    aboveColors = NULL,
    valueRange = NULL,
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
    m <- leaflet::fitBounds(
      map = m, 
      lng1 = raster::extent(r_wgs84)[1],
      lng2 = raster::extent(r_wgs84)[2], 
      lat1 = raster::extent(r_wgs84)[3],
      lat2 = raster::extent(r_wgs84)[4])
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
        if(highestValue <= max(aboveValues)){
          highestValue <- NULL
        }
      }
      cuts <- c(aboveValues, highestValue) #set breaks
      r_factor <- cut(
        x = raster::values(r_wgs84), 
        breaks = cuts, 
        right = TRUE, 
        include.lowest = TRUE
      )
      raster::values(r_wgs84) <- r_factor
      hexCols <- apply(col2rgb(aboveColors), MARGIN = 2, function(co){
        rgb(red = co[1], green = co[2], blue = co[3], alpha = 0, maxColorValue = 255)
      })
      palData  <- leaflet::colorFactor(palette = hexCols, 
                                       domain = seq_along(levels(r_factor)), 
                                       na.color = "#00000000")
      palLegend <- leaflet::colorFactor(palette = hexCols, 
                                        domain = r_factor, 
                                        na.color = "#00000000")
    } else {
      if(is.null(valueRange)){
        valueRange <- range(raster::values(r_wgs84), na.rm = TRUE)
        valueRange <- valueRange + (diff(valueRange) * c(-0.00001, 0.00001))
        plotLegend <- FALSE
      }
      pal <- leaflet::colorNumeric(
        palette = c("white", "black"), 
        domain = valueRange)
    }
    
    m <- leaflet::leaflet()
    m <- leaflet::fitBounds(
      map = m, 
      lng1 = raster::extent(r_wgs84)[1],
      lng2 = raster::extent(r_wgs84)[2], 
      lat1 = raster::extent(r_wgs84)[3],
      lat2 = raster::extent(r_wgs84)[4])
    
    # m <- leaflet::setView(
    #   map = m, 
    #   lng = mean(raster::extent(r_wgs84)[1:2]), 
    #   lat = mean(raster::extent(r_wgs84)[3:4]), 
    #   zoom = 14
    # )
    m <- leaflet::addProviderTiles(map = m, provider = "OpenStreetMap.Mapnik")
    #m <- leaflet::addTiles(map = m, layerId = "Esri.WorldTopoMap")
    m <- leaflet::addRasterImage(
      map = m, 
      x = r_wgs84, 
      colors = palData, 
      opacity = 1
    )  
    if(plotLegend){
      if(!is.null(aboveValues)){
        legend_i <- seq_along(aboveValues)
        if(length(aboveValues) > 10){
          legend_i <- round(seq(1, length(aboveValues), length.out = 10))
        }
        m <- leaflet::addLegend(
          map = m, 
          values = r_factor,
          pal = palLegend,
          #colors = leaflet::colorBin(domain = aboveValues, palette = hexCols[legend_i]), 
          labels = levels(r_factor)[legend_i],
          title = legendTitle)
      } else {
        m <- leaflet::addLegend(
          map = m, 
          pal = pal, 
          values = c(valueRange[1], mean(valueRange), valueRange[2]),
          title = legendTitle)
      }
    }
  }
  m
}



#' Plot one layer (Matrix) described by a netCDF
#' 
#' The plot is the layer on top of a leaflet map. The color scheme can
#' be done for numeric or categorical values.
#' 
#' @param ncLayer This is a matrix that corresponds to the x and y dimensions of
#' the netCDF file. It can be a band of netCDF or further processed layer. It
#' contant of the matrix can either be numerical or characters of hex colors.
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param lowerLimits Lower limits of value classes. If NULL (default) colors
#' will be scaled according to numerical values. 
#' @param highestValue If Null the highest value is derived by the maximum of
#' all available layer values. It can also be specified manually.
#' @param classColors A vector of R-colors names corresponding to the value
#' classes. 
#' @param valueRange Minimum and maximum values used for color scale of numeric 
#' values.
#' @param legendTitle Character string for legend title
#' @param plotLegend If TRUE legend will be plotted. If now aboveValues and no
#' valueRange are spefecied. No legend will be plotted.
#' 
#' @details
#' Use case: Proportions of a scene.
#' The above values and correspoding colors need to be specified. If for example 
#' values between 0 and 0.5 (50%) should bewhite, values between 0.5 and 0.75 
#' should be yellow and values above 0.75 should be purple, aboveValues are 
#' c(0, 0.5, 0.75) and the aboveColors arec("white", "yellow", "purple"). 
#' In that case a legendTitle is recommended to explain what is displayed.
#' 
#' @importFrom raster raster image
#' @importFrom graphics arrows
#' @importFrom geosphere distGeo
#' 
#' @export
#' 
plot_layer <- function(
    ncLayer, 
    nc,
    lowerLimits = NULL,
    classColors = NULL,
    highestValue = NULL,
    valueRange = NULL,
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
  
  if(hexValues){
    r_factor <- as.factor(raster::values(r))
    raster::values(r) <- r_factor
    pal <- leaflet::colorFactor(palette = as.vector(COLOR), 
                                domain = seq_along(levels(r_factor)), 
                                na.color = "#00000000")
    
  } else {
    if(!is.null(lowerLimits)){
      if(is.null(highestValue)){
        highestValue <- max(raster::values(r), na.rm = TRUE)
        if(highestValue <= max(lowerLimits)){
          highestValue <- NULL
        }
      }
      cuts <- c(lowerLimits, highestValue) #set breaks
      
      LegendText <- levels(cut(
        x = 0, 
        breaks = cuts, 
        include.lowest = TRUE))
    } else {
      if(is.null(valueRange)){
        valueRange <- range(raster::values(r), na.rm = TRUE)
        valueRange <- valueRange + (diff(valueRange) * c(-0.00001, 0.00001))
        cuts <- valueRange
        plotLegend <- FALSE
        classColors <- c("white", "black")
      }
    }
    raster::values(r) <- cut(
      x = as.numeric(raster::values(r)), 
      breaks = cuts, 
      include.lowest = TRUE)
    foundLevels <- unique(raster::values(r))
    
  }
  
  bbox <- matrix(data = raster::extent(to_wgs84),
                 nrow = 2)

  x_meters <- geosphere::distGeo(
    p1 = c(bbox[1,1], mean(bbox[,2])), 
    p2 = c(bbox[2,1], mean(bbox[,2])))
  y_meters <- geosphere::distGeo(
    p1 = c(mean(bbox[,1]), bbox[1,2]), 
    p2 = c(mean(bbox[,1]), bbox[2,2]))
 
  plot_scale(pin_w = x_meters, pin_h = y_meters)
  
  dy <- diff(range(nc$y))
  dx <- diff(range(nc$x))
  idt <- 0.03
  
  raster::image(
    r, col = classColors[sort(foundLevels)], 
    xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE)
  
  arrows(x0 = min(nc$x), x1 = max(nc$x), 
         y0 = min(nc$y) + idt * dy, y1 = min(nc$y) + idt * dy, 
         lwd = 2, length = 0.1)
  text(x = max(nc$x) - 3 * idt * dx, y = min(nc$y) + idt * dy, 
       labels = paste0(signif(x_meters / 1000, 2), " km"), pos = 3)
  
  arrows(x0 = min(nc$x) + idt * dx, x1 = min(nc$x) + idt * dx, 
         y0 = min(nc$y), y1 = max(nc$y), 
         lwd = 2, length = 0.1)
  text(x = min(nc$x) + idt * dx, y = max(nc$y) - idt * dy, 
       labels = paste0(signif(y_meters / 1000, 2), " km"), pos = 4)
  
  if(plotLegend){
    legend(
      x =  par("usr")[2] + diff(par("usr")[1:2]) / diff(par("plt")[1:2]) * (1 - par("plt")[2]),
      y =  par("usr")[4] + diff(par("usr")[3:4]) / diff(par("plt")[3:4]) * (1 - par("plt")[4]), 
      legend = LegendText, 
      fill = classColors, 
      bg = rgb(1,1,1, 0.5),
      title = legendTitle, 
      box.col = rgb(1,1,1, 0.5), 
      xpd = TRUE, 
      xjust = 1, 
      yjust = 1
    )
  }
}

#' Define plot region scale
#' @param pin_w widthsize 
#' @param pin_h hightsize
#' 
#' @details
#' both values will be treated relatively and will adjust the plot margin to 
#' the device size
#' 
#' 
plot_scale <- function(pin_w, pin_h) {
  din <- par("din")  # Device-Größe in inches [Breite, Höhe]
  scale_w <- din[1] / pin_w
  scale_h <- din[2] / pin_h
  scale <- min(scale_w, scale_h)  # Skalierung zum "Anstoßen"
  pin_w_s <- pin_w * scale
  pin_h_s <- pin_h * scale
  mai_lr <- (din[1] - pin_w_s) / 2  # Symmetrisch links/rechts
  mai_bt <- (din[2] - pin_h_s) / 2  # Symmetrisch unten/oben
  mai <- c(mai_bt, mai_lr, mai_bt, mai_lr)
  par(pin = c(pin_w_s, pin_h_s), mai = mai)
}

