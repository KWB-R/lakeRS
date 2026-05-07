#' Leaflet Map one layer (Matrix) described by a netCDF
#' 
#' The plot is the layer on top of a leaflet map. The color scheme can
#' be done for numeric or categorical values.
#' 
#' @param ncLayer This is a layer that corresponds to the x and y dimensions of
#' the netCDF file. It can be a band of netCDF or further processed layer
#' created by [waterscene_proportion()]
#' @param nc The netCDF data list created by [open_netcdf()]
#' @param lowerLimits Lower limits of value classes. If NULL (default) 
#' numerical values will be used.
#' @param highestValue If Null the highest value is derived by the maximum of
#' all available layer values. It can also be specified manually.
#' @param classColors A vector of R-colors names corresponding to the value
#' classes. 
#' @param valueRange Minimum and maximum values used for color scale of numeric 
#' values.
#' @param legendTitle Character string for legend title
#' @param plotLegend If TRUE legend will be plotted. If now lowerLimits and no
#' valueRange are spefecied. No legend will be plotted.
#' 
#' @details
#' Use case: Proportions of a scene.
#' The above values and correspoding colors need to be specified. If for example 
#' values between 0 and 0.5 (50%) should bewhite, values between 0.5 and 0.75 
#' should be yellow and values above 0.75 should be purple, lowerLimits are 
#' c(0, 0.5, 0.75) and the classColors arec("white", "yellow", "purple"). 
#' In that case a legendTitle is recommended to explain what is displayed.
#' 
#' 
#' 
#' @importFrom terra rast values ext
#' @importFrom leaflet colorFactor colorNumeric leaflet setView addTiles addRasterImage addLegend
#' 
#' @export
#' 
map_layer <- function(
    ncLayer, 
    nc,
    lowerLimits = NULL,
    highestValue = NULL,
    classColors = NULL,
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
  
  r_wgs84 <- sameDimProjection(
    initial_raster = terra::rast(
      x = ncLayer,
      extent = c(min(nc$x), max(nc$x), min(nc$y), max(nc$y)),
      crs    = nc$crs), 
    finalCRS = "EPSG:4326")
  
  if(hexValues){
    r_factor <- as.factor(terra::values(r_wgs84))
    terra::values(r_wgs84) <- r_factor
    pal <- leaflet::colorFactor(palette = as.vector(COLOR), 
                                domain = seq_along(levels(r_factor)), 
                                na.color = "#00000000")
    
    m <- leaflet::leaflet()
    m <- leaflet::fitBounds(
      map = m, 
      lng1 = unname(terra::ext(r_wgs84)[1]),
      lng2 = unname(terra::ext(r_wgs84)[2]), 
      lat1 = unname(terra::ext(r_wgs84)[3]),
      lat2 = unname(terra::ext(r_wgs84)[4]))
    m <- leaflet::addProviderTiles(map = m, provider = "OpenStreetMap.Mapnik")
    m <- leaflet::addRasterImage(
      map = m, 
      x = r_wgs84, 
      colors = pal, 
      opacity = 0.8
    )
  } else {
    if(!is.null(lowerLimits)){
      if(is.null(highestValue)){
        highestValue <- max(terra::values(r_wgs84), na.rm = TRUE)
        if(highestValue <= max(lowerLimits)){
          highestValue <- NULL
        }
      }
      cuts <- c(lowerLimits, highestValue) #set breaks
      r_factor <- cut(
        x = terra::values(r_wgs84), 
        breaks = cuts, 
        right = TRUE, 
        include.lowest = TRUE
      )
      terra::values(r_wgs84) <- r_factor
      hexCols <- apply(col2rgb(classColors), MARGIN = 2, function(co){
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
        valueRange <- range(terra::values(r_wgs84), na.rm = TRUE)
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
      lng1 = unname(terra::ext(r_wgs84)[1]),
      lng2 = unname(terra::ext(r_wgs84)[2]), 
      lat1 = unname(terra::ext(r_wgs84)[3]),
      lat2 = unname(terra::ext(r_wgs84)[4]))
    m <- leaflet::addProviderTiles(map = m, provider = "OpenStreetMap.Mapnik")
    
    m <- leaflet::addRasterImage(
      map = m, 
      x = r_wgs84, 
      colors = palData, 
      opacity = 1
    )  
    if(plotLegend){
      if(!is.null(lowerLimits)){
        legend_i <- seq_along(lowerLimits)
        if(length(lowerLimits) > 10){
          legend_i <- round(seq(1, length(lowerLimits), length.out = 10))
        }
        m <- leaflet::addLegend(
          map = m, 
          values = r_factor,
          pal = palLegend,
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
#' @param nc The netCDF data list created by [open_netcdf()]
#' @param lowerLimits Lower limits of value classes. If NULL (default) colors
#' will be scaled according to numerical values. 
#' @param highestValue If Null the highest value is derived by the maximum of
#' all available layer values. It can also be specified manually.
#' @param classColors A vector of R-colors names corresponding to the value
#' classes. 
#' @param valueRange Minimum and maximum values used for color scale of numeric 
#' values.
#' @param legendTitle Character string for legend title
#' @param plotLegend If TRUE legend will be plotted. If no lowerLimits and no
#' valueRange are specefied. No legend will be plotted.
#' 
#' @details
#' Use case: Proportions of a scene.
#' The above values and correspoding colors need to be specified. If for example 
#' values between 0 and 0.5 (50%) should bewhite, values between 0.5 and 0.75 
#' should be yellow and values above 0.75 should be purple, lowerLimits are 
#' c(0, 0.5, 0.75) and the classColors arec("white", "yellow", "purple"). 
#' In that case a legendTitle is recommended to explain what is displayed.
#' 
#' @importFrom terra rast values ext image
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
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
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
  
  r_wgs84 <- sameDimProjection(
    initial_raster = terra::rast(
      x = ncLayer,
      extent = c(min(nc$x), max(nc$x), min(nc$y), max(nc$y)),
      crs    = nc$crs), 
    finalCRS = "EPSG:4326")
  
  if(hexValues){
    r_factor <- as.factor(terra::values(r_wgs84))
    terra::values(r_wgs84) <- r_factor
    pal <- leaflet::colorFactor(palette = as.vector(COLOR), 
                                domain = seq_along(levels(r_factor)), 
                                na.color = "#00000000")
    
  } else {
    if(!is.null(lowerLimits)){
      if(is.null(highestValue)){
        highestValue <- max(terra::values(r_wgs84), na.rm = TRUE)
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
        valueRange <- range(terra::values(r_wgs84), na.rm = TRUE)
        valueRange <- valueRange + (diff(valueRange) * c(-0.00001, 0.00001))
        cuts <- valueRange
        plotLegend <- FALSE
        classColors <- c("white", "black")
      }
    }
    terra::values(r_wgs84) <- cut(
      x = as.numeric(terra::values(r_wgs84)), 
      breaks = cuts, 
      include.lowest = TRUE)
    foundLevels <- unique(terra::values(r_wgs84))
    
  }
  
  bbox <- matrix(data = terra::ext(r_wgs84),
                 nrow = 2)
  x_meters <- geosphere::distGeo(
    p1 = c(bbox[1,1], mean(bbox[,2])), 
    p2 = c(bbox[2,1], mean(bbox[,2])))
  y_meters <- geosphere::distGeo(
    p1 = c(mean(bbox[,1]), bbox[1,2]), 
    p2 = c(mean(bbox[,1]), bbox[2,2]))
 
  plot_scale(pin_w = x_meters, pin_h = y_meters)
  
  dy <- diff(bbox[,2])
  dx <- diff(bbox[,1])
  idt <- 0.03
  
  terra::image(
    r_wgs84, col = classColors[sort(foundLevels)], 
    xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE)
  
  arrows(x0 = bbox[1,1], x1 = bbox[2,1], 
         y0 = bbox[1,2] + idt * dy, y1 = bbox[1,2] + idt * dy, 
         lwd = 2, length = 0.1)
  text(x = bbox[2,1] - 3 * idt * dx, y = bbox[1,2] + idt * dy, 
       labels = paste0(signif(x_meters / 1000, 2), " km"), pos = 3)
  
  arrows(x0 =  bbox[1,1] + idt * dx, x1 =  bbox[1,1] + idt * dx, 
         y0 = bbox[1,2], y1 = bbox[2,2], 
         lwd = 2, length = 0.1)
  text(x =  bbox[1,1] + idt * dx, y = bbox[2,2] - idt * dy, 
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
  din <- par("din")  # Device-size in inch
  scale_w <- din[1] / pin_w
  scale_h <- din[2] / pin_h
  scale <- min(scale_w, scale_h)  # scale
  pin_w_s <- pin_w * scale
  pin_h_s <- pin_h * scale
  mai_lr <- (din[1] - pin_w_s) / 2  # Symmetrical left/right
  mai_bt <- (din[2] - pin_h_s) / 2  # Symmetrical bottom/top
  mai <- c(mai_bt, mai_lr, mai_bt, mai_lr)
  if(any(mai < 0)){
    dev.new()
    din <- par("din")  # Device-size in inch
    scale_w <- din[1] / pin_w
    scale_h <- din[2] / pin_h
    scale <- min(scale_w, scale_h)  # scale
    pin_w_s <- pin_w * scale
    pin_h_s <- pin_h * scale
    mai_lr <- (din[1] - pin_w_s) / 2  # Symmetrical left/right
    mai_bt <- (din[2] - pin_h_s) / 2  # Symmetrical bottom/top
    mai <- c(mai_bt, mai_lr, mai_bt, mai_lr)
  }
  par(pin = c(pin_w_s, pin_h_s), mai = mai)
}
