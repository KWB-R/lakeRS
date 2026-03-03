#' #' Plot the moving average dynamic of an index
#' #' 
#' #' @param DynamicStatList A list of Index dynamic statistics (mean, median, 
#' #' quantiles 5%, 25%, 75% an 95%) as part of the output of [dynamic_per_pixel()]
#' #' @param timeseriesNames String defining the name of the timeseries provided in 
#' #' DynamicStatList. This is used for the legend only.
#' #' @param lakeName String defining the title of the plot
#' #' @param ylab y axis name
#' #' @param ylim Optional manual definition of the y-axes dimension.
#' #' @param smallBandOnly smallBandOnly TRUE means that only the 50% range of data 
#' #' (25th to 75th percentile) and median value are plotted.
#' #' 
#' #' @details
#' #' A maximum of 5000 timeseries will be drawn as single lines. If more data is 
#' #' available 5000 timeseries are randomly drawn. The transparency of each single line
#' #' depends on the number of lines to be drawn (the more lines, the higher the
#' #' transparency)
#' #' 
#' #' @export
#' #' 
#' plot_dynamic <- function(
#'     DynamicStatList, timeseriesNames = 1:length(DynamicStatList), 
#'     lakeName = "", ylab = "Index", ylim = NULL, smallBandOnly = TRUE
#' ){
#'   
#'   cv <- lakeRS::tenClusterColors$color
#'   polygon_color <- apply(X = col2rgb(cv), 
#'                          MARGIN = 2, 
#'                          FUN = function(x){
#'                            rgb(red = x["red"], green = x["green"], blue = x["blue"], 
#'                                alpha = 80, maxColorValue = 255)
#'                          }
#'   )
#'   
#'   v_lines <- as.numeric(format(x = as.Date(
#'     paste("2025", 1:11, c(31,28,31,30,31,30,31,31,30,31,30), 
#'           sep = "-")),
#'     "%j")
#'   )
#'   m_position <- as.numeric(format(x = as.Date(
#'     paste("2025", 1:12, 15, 
#'           sep = "-")),
#'     "%j")
#'   )
#'   m_text <- format(x = as.Date(
#'     paste("2025", 1:12, 15, 
#'           sep = "-")),
#'     "%b")
#'   
#'   if(is.null(ylim)){
#'     if(smallBandOnly){
#'       yrange <- range(unlist(lapply(DynamicStatList, function(x){
#'         range(
#'           c(x$q_0.25, x$q_0.75), 
#'           na.rm = TRUE
#'         )
#'       })))
#'     } else {
#'       yrange <- range(unlist(lapply(DynamicStatList, function(x){
#'         range(
#'           c(x$q_0.05, x$q_0.95), 
#'           na.rm = TRUE
#'         )
#'       })))
#'     }
#'     ylim <- c(yrange[1] - diff(yrange) * 0.1, yrange[2] + diff(yrange) * 0.1)
#'   }
#'   plot(x = 1:365, y = rep(0,365), type = "n", 
#'        ylab = ylab, xlab = "Day of the Year", 
#'        ylim = ylim, 
#'        main = paste0(lakeName, ifelse(
#'          smallBandOnly,  
#'          yes = " (Median and 50% Interval)", 
#'          no = " (Median, 90% and 50% Interval)")), 
#'        xaxs = "i")
#'   
#'   abline(v = v_lines, lty = "dashed")
#'   mtext(text = m_text, side = 3, line = 0.5, at = m_position)
#'   
#'   for(i in seq_along(DynamicStatList)){
#'     value_stats <- DynamicStatList[[i]]
#'     if(!smallBandOnly){
#'       xy <- rm_na_for_polygon(
#'         x_v = c(1:365, 365:1), 
#'         y_v = c(value_stats$q_0.05, rev(value_stats$q_0.95))
#'       )
#'       polygon(x = xy$x, y = xy$y, border = NA, col = polygon_color[i])
#'     }
#'     xy <- rm_na_for_polygon(
#'       x_v = c(1:365, 365:1), 
#'       y_v = c(value_stats$q_0.25, rev(value_stats$q_0.75))
#'     )
#'     polygon(x = xy$x, y = xy$y, border = NA, col = polygon_color[i])
#'     lines(x = 1:365, y = value_stats$q_0.5, lwd = 2, col = cv[i])
#'   }
#'   
#'   sapply(DynamicStatList, length)
#'   legend(
#'     "top",
#'     legend = paste0("TS ", timeseriesNames),
#'     fill = cv[1:length(DynamicStatList)],
#'     bg = "black",
#'     box.lwd = NA,
#'     cex = 0.9,
#'     text.col = "white",
#'     ncol = ifelse(length(DynamicStatList) < 5, length(DynamicStatList), 5)
#'   )
#' }
#' 
#' 
#' #' Remove NA values for polygon plotting
#' #' 
#' #' @param x_v,y_v vectors of x and y values of the polygon
#' #' 
#' rm_na_for_polygon <- function(x_v, y_v){
#'   if(any(is.na(y_v))){
#'     x_v <- x_v[!is.na(y_v)]
#'     y_v <- y_v[!is.na(y_v)]
#'   }
#'   if(any(is.na(x_v))){
#'     y_v <- y_v[!is.na(x_v)]
#'     x_v <- x_v[!is.na(x_v)]
#'   }
#'   list("x" = x_v,
#'        "y" = y_v)
#' }
#' 
