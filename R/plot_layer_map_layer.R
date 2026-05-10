#' Display a netCDF-aligned matrix as an interactive leaflet layer
#'
#' Reprojects a matrix aligned with an [open_netcdf()] object to WGS 84 and adds
#' it as a raster overlay on an interactive leaflet map.
#'
#' @param ncLayer Matrix with the same row and column dimensions as the netCDF
#'   grid. Values can be numeric classes, continuous numeric values, or hex color
#'   strings.
#' @param nc A list returned by [open_netcdf()]. Used for coordinates and CRS.
#' @param lowerLimits Optional numeric vector of lower class limits. If supplied,
#'   categorical intervals are created with [cut()].
#' @param highestValue Optional numeric upper bound appended to `lowerLimits`.
#'   If `NULL`, the maximum layer value is used.
#' @param classColors Character vector of colors corresponding to the classes
#'   defined by `lowerLimits`.
#' @param valueRange Optional numeric vector of length two for continuous color
#'   scaling.
#' @param legendTitle Optional character scalar used as legend title.
#' @param plotLegend Logical. If `TRUE`, a leaflet legend is added when enough
#'   information is available.
#'
#' @return A `leaflet` map object.
#'
#' @details The function supports a hex-color mode when all values of `ncLayer`
#'   are character strings beginning with `#`.
#'
#' @importFrom terra rast values ext
#' @importFrom leaflet colorFactor colorNumeric leaflet fitBounds addProviderTiles addRasterImage addLegend
#' @importFrom grDevices col2rgb rgb
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
  v <- terra::values(r_wgs84)
  
  if(hexValues){
    r_factor <- factor(v, exclude = c(NA, NaN))
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
    foundLevels <- sort(unique(v[!is.na(v)]))
    if(!is.null(lowerLimits)){
      if(is.null(highestValue)){
        highestValue <- max(terra::values(r_wgs84), na.rm = TRUE)
        if(highestValue <= max(lowerLimits)){
          highestValue <- NULL
        }
      }
      if(all(foundLevels %in% lowerLimits)){ # already classes -> as factor
        r_factor <- factor(v, exclude = c(NA,NaN))
      } else {
        cuts <- c(lowerLimits, highestValue) #set breaks
        r_factor <- cut(
          x = terra::values(r_wgs84), 
          breaks = cuts, 
          right = TRUE, 
          include.lowest = TRUE
        )
      }
      terra::values(r_wgs84) <- r_factor
     
     
      hexCols <- apply(col2rgb(classColors)[,seq_along(levels(r_factor))], MARGIN = 2, function(co){
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
      palData <- palLegend <- leaflet::colorNumeric(
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
          pal = palLegend, 
          values = c(valueRange[1], mean(valueRange), valueRange[2]),
          title = legendTitle)
      }
    }
  }
  m
}

#' Plot a netCDF-aligned matrix as a static map
#'
#' Reprojects a matrix aligned with a netCDF grid to WGS 84 and plots it with a
#' distance scale and optional legend using base graphics.
#'
#' @param ncLayer Matrix with the same row and column dimensions as the netCDF
#'   grid. Values can be numeric or hex color strings.
#' @param nc A list returned by [open_netcdf()]. Used for coordinates and CRS.
#' @param lowerLimits Optional numeric vector of class lower limits.
#' @param classColors Character vector of colors for classes. In continuous mode
#'   without `valueRange`, defaults internally to `c("white", "black")`.
#' @param highestValue Optional upper bound for the last class.
#' @param valueRange Optional numeric vector of length two for continuous value
#'   scaling.
#' @param legendTitle Optional character scalar used as legend title.
#' @param plotLegend Logical. If `TRUE`, a legend is plotted when class labels
#'   are available.
#'
#' @return No explicit return value. The function draws a base R plot.
#'
#' @details The plot is scaled according to geodesic width and height estimated
#'   with [geosphere::distGeo()]. The helper [plot_scale()] adjusts the plot
#'   region to preserve the map's aspect ratio.
#'
#' @importFrom terra rast values ext image
#' @importFrom graphics arrows legend par text
#' @importFrom grDevices col2rgb rgb
#' @importFrom geosphere distGeo
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
  v <- terra::values(r_wgs84)
  
  if(hexValues){ # Already colors -> nothing else needed
    r_factor <- as.factor(v)
    terra::values(r_wgs84) <- r_factor
    pal <- leaflet::colorFactor(palette = as.vector(COLOR), 
                                domain = seq_along(levels(r_factor)), 
                                na.color = "#00000000")
    
  } else {
    foundLevels <- sort(unique(v[!is.na(v)]))
    if(!is.null(lowerLimits)){
      if(is.null(highestValue)){
        highestValue <- max(v, na.rm = TRUE)
        if(highestValue <= max(lowerLimits)){
          highestValue <- NULL
        }
      }
      if(all(foundLevels %in% lowerLimits)){ # already classes -> as factor
        LegendText <- foundLevels
        terra::values(r_wgs84)[!is.na(v)] <- factor(v[!is.na(v)])
      } else { # numeric data -> derive classes 
        cuts <- c(lowerLimits, highestValue) 
        LegendText <- levels(cut(
          x = 0, 
          breaks = cuts, 
          include.lowest = TRUE))
        terra::values(r_wgs84) <- cut(
          x = as.numeric(v), 
          breaks = cuts, 
          include.lowest = TRUE)
        foundLevels <- sort(unique(
          terra::values(r_wgs84)[!is.na(terra::values(r_wgs84))]
        ))
      }
    } else { 
      if(is.null(valueRange)){
        valueRange <- range(v, na.rm = TRUE)
        valueRange <- valueRange + (diff(valueRange) * c(-0.00001, 0.00001))
        cuts <- valueRange
        plotLegend <- FALSE
        classColors <- c("white", "black")
      }
      terra::values(r_wgs84) <- cut(
        x = as.numeric(terra::values(r_wgs84)), 
        breaks = cuts, 
        include.lowest = TRUE)
      foundLevels <- c(1,2)
    }
    
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
    if(length(foundLevels) > 15){
      foundLevels <- pretty(foundLevels, n = 15)
    }
    legend(
      x =  par("usr")[2] + diff(par("usr")[1:2]) / diff(par("plt")[1:2]) * (1 - par("plt")[2]),
      y =  par("usr")[4] + diff(par("usr")[3:4]) / diff(par("plt")[3:4]) * (1 - par("plt")[4]), 
      legend = LegendText[foundLevels], 
      fill = classColors[foundLevels], 
      bg = rgb(1,1,1, 0.5),
      title = legendTitle, 
      box.col = rgb(1,1,1, 0.5), 
      xpd = TRUE, 
      xjust = 1, 
      yjust = 1
    )
  }
}

#' Adjust base-graphics plot size to a target aspect ratio
#'
#' Sets the graphics parameters `pin` and `mai` so that a plot region matches a
#' target width-to-height ratio while remaining centered in the device.
#'
#' @param pin_w Numeric scalar. Relative target width.
#' @param pin_h Numeric scalar. Relative target height.
#'
#' @return The result of [graphics::par()] is returned invisibly by `par()`.
#'
#' @details If the current graphics device is too small to accommodate the
#'   requested region, a new device is opened and the scaling is recalculated.
#'
#' @keywords internal
#' 
plot_scale <- function(
    pin_w, pin_h)
{
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

