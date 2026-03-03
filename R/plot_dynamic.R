#' Plot dynamic time series with uncertainty intervals for environmental data
#'
#' Creates a base R plot visualizing multiple time series (medians/averages) 
#' over 365 days of the year,with optional 50% and/or 95% quantile intervals as 
#' shaded polygons, monthly separators, and an optional reference line 
#' (e.g., pixel data). 
#' Auto-generates quantile data.frames from mean/sd.
#'
#' @param v_averageList List of numeric vectors (length 365); medians/averages 
#' per day of year. Single vectors are automatically wrapped in a list.
#' @param v_sdList List of numeric vectors (length 365), or NULL. Standard 
#' deviations per day. Auto-generates \code{df_q50List} (IQR via 0.674 x sd) and 
#' \code{df_q95List} (1.96 x sd).Single vectors coerced to lists. 
#' Default: NULL (no uncertainty shading).
#' @param df_q50List List of data.frames (365 rows, 2 cols: "q_0.25", "q_0.75"), 
#' or NULL/single data.frame.50% quantile intervals. Takes precedence if both 
#' quantile lists provided. 
#' @param df_q95List List of data.frames (365 rows, 2 cols: "q_0.025", "q_0.975"), 
#' or NULL/single data.frame. 95% quantile intervals. 
#' @param TScolors Character vector of colors for time series (default: 
#' \code{lakeRS::tenClusterColors$color}).Recycled to match \code{v_averageList} 
#' length.
#' @param TSnames Character vector of names for legend entries (time series), 
#' or empty string/NULL (no legend). Length should match \code{v_averageList}.
#' @param df_reference Data.frame (optional; cols: day 1-365, reference values), 
#' e.g., ground truth data.
#' @param RefName Character; legend name for reference line (dashed, black). 
#' Default: NULL.
#' @param lakeName Character; plot title prefix (e.g., lake identifier). 
#' Default: "".
#' @param ylab Character; y-axis label (e.g., "Chlorophyll-a Index"). 
#' Default: "Index".
#' @param ylim Numeric vector (2 elements); manual y-limits. 
#' Auto-computed with 10% padding if NULL.
#'
#' @return Invisible NULL (plots directly; no return value). 
#' Legend positioned dynamically at top-left.
#' 
#' @export
#'
plot_dynamic <- function(
    v_averageList, v_sdList = NULL, df_q50List = NULL, df_q95List = NULL, 
    TScolors = lakeRS::tenClusterColors$color, TSnames = NULL, 
    df_reference = NULL, RefName = NULL, 
    lakeName = "", ylab = "Index", ylim = NULL
){
  if(!is.list(v_averageList)){
    v_averageList <- list(v_averageList)
  }
  if(!is.list(v_sdList) & !is.null(v_sdList)){
    v_sdList <- list(v_sdList)
  }
  if(is.data.frame(df_q50List) & !is.null(df_q50List)){
    df_q50List <- list(df_q50List)
  }
  if(is.data.frame(df_q95List) & !is.null(df_q95List)){
    df_q95List <- list(df_q95List)
  }
  
  
  if(!is.null(v_sdList)){
    if(length(v_averageList) != length(v_sdList)){
      stop("Length of v_averageList and v_sdList need to be equal if v_sdList is given.")
    }
    df_q50List <-lapply(seq_along(v_sdList), function(i){
      data.frame(
        "q_0.25" = v_averageList[[i]] - 0.674 * v_sdList[[i]],
        "q_0.75" = v_averageList[[i]] + 0.674 * v_sdList[[i]]
      )
    })
    df_q95List <-lapply(seq_along(v_sdList), function(i){
      data.frame(
        "q_0.025" = v_averageList[[i]] - 1.96 * v_sdList[[i]],
        "q_0.975" = v_averageList[[i]] + 1.96 * v_sdList[[i]]
      )
    })
  }
  plotTitle <-lakeName
  if(!is.null(df_q95List)){
    plotTitle <- paste0(plotTitle, ifelse(
      test = is.null(df_q50List), 
      yes = " (Median and 95% Interval)", 
      no = " (Median, 95% and 50% Interval)"))
    yrange <- range(unlist(df_q95List), na.rm = TRUE)
  } else if(!is.null(df_q50List)){
    plotTitle <- paste0(plotTitle, " (Median and 50% Interval)")
    yrange <- range(unlist(df_q50List), na.rm = TRUE)
  } else {
    yrange <- range(unlist(v_averageList), na.rm = TRUE)
  }
  
  if(is.null(ylim)){
    ylim <- c(yrange[1] - diff(yrange) * 0.1, yrange[2] + diff(yrange) * 0.1)
  }
  
  polygon_colors <- apply(X = col2rgb(TScolors), 
                          MARGIN = 2, 
                          FUN = function(x){
                            rgb(red = x["red"], green = x["green"], blue = x["blue"], 
                                alpha = 80, maxColorValue = 255)
                          }
  )
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
  
  plot(x = 1:365, y = rep(0,365), 
       type = "n", 
       ylab = ylab, 
       xlab = "Day of the Year", 
       ylim = ylim, 
       main = plotTitle, 
       xaxs = "i"
  )
  
  abline(v = v_lines, lty = "dashed")
  mtext(text = m_text, side = 3, line = 0.5, at = m_position)
  
  if(!is.null(df_q95List)){
    for(i in seq_along(df_q95List)){
      xy <- rm_na_for_polygon(
        x_v = c(1:365, 365:1), 
        y_v = c(df_q95List[[i]][,1], rev(df_q95List[[i]][,2]))
      )
      polygon(x = xy$x, y = xy$y, border = NA, col = polygon_colors[i])
    }
    
  }
  if(!is.null(df_q50List)){
    for(i in seq_along(df_q50List)){
      xy <- rm_na_for_polygon(
        x_v = c(1:365, 365:1), 
        y_v = c(df_q50List[[i]][,1], rev(df_q50List[[i]][,2]))
      )
      polygon(x = xy$x, y = xy$y, border = NA, col = polygon_colors[i])
    }
  }
  
  for(i in seq_along(v_averageList)){
    lines(x = 1:365, y = v_averageList[[i]], lwd = 2, col = TScolors[i])
  }
  if(!is.null(df_reference)){
    lines(x = df_reference[,1], y = df_reference[,2], lwd = 2, lty = "dashed") 
  }
  
  if(!is.null(TSnames)){
    l <- legend(
      "topleft",
      legend = TSnames[1],
      fill = TScolors[1],
      bg = "black",
      box.lwd = NA,
      cex = 0.9,
      text.col = "white", )
    for(i in seq_along(v_averageList)[-1]){
      l <- legend(
        x = l$rect$left + l$rect$w,
        y = l$rect$top,
        legend = TSnames[i],
        fill = TScolors[i],
        bg = "black",
        box.lwd = NA,
        cex = 0.9,
        text.col = "white")
    }
  }
  if(!is.null(RefName)){
    if(exists("l")){
      legend(
        x = l$rect$left + l$rect$w,
        y = l$rect$top,
        legend = RefName,
        lty = "dashed", lwd = 2,
        box.lwd = 1,
        cex = 0.9,
        text.col = "black")
    } else {
      legend(
        "topright",
        legend = RefName,
        lty = "dashed", lwd = 2,
        box.lwd = 1,
        cex = 0.9,
        text.col = "black")
    }
  }
}

#' Remove NA values symmetrically for polygon plotting
#'
#' Prepares x/y vectors for \code{\link[graphics:polygon]{polygon()}} by 
#' removing corresponding NAs from either vector while preserving order. Ensures 
#' closed polygons plot correctly despite missing data. Used internally by 
#' \code{\link{plot_dynamic}} for shaded uncertainty regions.
#'
#' @param x_v Numeric vector; x-coordinates 
#' @param y_v Numeric vector; y-coordinates (same length as \code{x_v})
#'
#' @return List with \code{x}, \code{y} (NA-stripped vectors of equal length).
#'
rm_na_for_polygon <- function(x_v, y_v){
  if(any(is.na(y_v))){
    x_v <- x_v[!is.na(y_v)]
    y_v <- y_v[!is.na(y_v)]
  }
  if(any(is.na(x_v))){
    y_v <- y_v[!is.na(x_v)]
    x_v <- x_v[!is.na(x_v)]
  }
  list("x" = x_v,
       "y" = y_v)
}

