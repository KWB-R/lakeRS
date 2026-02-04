#' Plot the moving average dynamic of an index
#' 
#' @param dpp The dynamic per pixel output by [dynamic_per_pixel()]
#' @param lakeName String defining the title of the plot
#' @param ylab y axis name
#' @param pixelClusters Optional: Named Integer vector of clusters. Names 
#' according to the pixel names (column names in the dpp$moving_average table). 
#' If pixelClusters are provided the dynamic is plotted for each cluster 
#' separately.
#' @param ylim Optional manual definition of the y-axes dimension.
#' @param smallBandOnly If pixel clusters are provided, smallBandOnly TRUE means
#' that only the 50% range of data (25th to 75th percentile) and median value 
#' are plotted.
#' 
#' @details
#' A maximum of 5000 timeseries will be drawn as single lines. If more data is 
#' available 5000 timeseries are randomly drawn. The transparency of each single line
#' depends on the number of lines to be drawn (the more lines, the higher the
#' transparency)
#' 
#' @export
#' 
plot_dynamic <- function(
    dpp, lakeName = "", ylab = "Index", pixelClusters = NULL, ylim = NULL, 
    smallBandOnly = FALSE
){
  
  cv <- lakeRS::tenClusterColors$color

  polygon_color <- apply(X = col2rgb(cv), 
        MARGIN = 2, 
        FUN = function(x){
          rgb(red = x["red"], green = x["green"], blue = x["blue"], 
              alpha = 80, maxColorValue = 255)
        }
  )
  
  all_values <- dpp$moving_averages[,-1]
  value_stats <- apply(all_values, 1, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
  
  # Plot Dynamic
  v_lines <- as.numeric(format(x = as.Date(
    paste("2025", 1:11, c(31,28,31,30,31,30,31,31,30,31,30), 
          sep = "-")),
    "%j")
  )
  m_position <- as.numeric(format(x = as.Date(
    paste("2025", 1:12, 15, 
          sep = "-")),
    "%j")
  )
  m_text <- format(x = as.Date(
    paste("2025", 1:12, 15, 
          sep = "-")),
    "%b")
  
  if(is.null(ylim)){
    if(smallBandOnly){
      ylim = range(value_stats) * c(1, 1.1)
    } else {
      ylim = range(all_values, na.rm = TRUE) * c(1, 1.1)
    }
  }
  plot(x = 1:365, y = all_values[,1], type = "n", 
       ylab = ylab, xlab = "Day of the Year", 
       ylim = ylim, 
       main = lakeName, xaxs = "i")
  
  abline(v = v_lines, lty = "dashed")
  mtext(text = m_text, side = 3, line = 0.5, at = m_position)
  if(is.null(pixelClusters)){
    polygon(x = c(1:365, 365:1), 
            y = c(value_stats["5%",], rev(value_stats["95%",])), 
            border = NA, col = polygon_color[1], )
    nLines <- ncol(all_values)
    i_cols <- 1:ncol(all_values) 
    if(nLines > 5000){
      i_cols <- sample(nLines, size = 5000)
      nLines <- length(i_cols)
    } 
    op <- 1 / (ifelse(nLines > 40, yes = nLines, no = 40)/40) 
    for(i in i_cols){
      lines(x = 1:365, y = all_values[,i], col = rgb(0,0,0,op))
    }
    
    lines(x = 1:365, y = value_stats["50%",], lwd = 2, col = cv[1])
  } else {
    cn <- sort(unique(pixelClusters))
    n_cn <- summary(as.factor(pixelClusters))
    for(cl_i in cn){
      all_values <- dpp$moving_averages[,names(pixelClusters)[pixelClusters == cl_i]]
      value_stats <- apply(all_values, 1, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
      
      if(!smallBandOnly){
        polygon(x = c(1:365, 365:1), 
                y = c(value_stats["5%",], rev(value_stats["95%",])), 
                border = NA, col = polygon_color[cl_i], ) 
      }
      polygon(x = c(1:365, 365:1), 
              y = c(value_stats["25%",], rev(value_stats["75%",])), 
              border = NA, col = polygon_color[cl_i], )
      lines(x = 1:365, y = value_stats["50%",], lwd = 2, col = cv[cl_i])
      
    }
    legend(
      "top", 
      legend = paste0("C", cn," (n=", format(n_cn, big.mark = ","), ")"), 
      fill = cv[cn], 
      horiz = TRUE, 
      bg = "black", 
      box.lwd = NA, 
      cex = 0.9, 
      text.col = "white"
    )
  }
}

