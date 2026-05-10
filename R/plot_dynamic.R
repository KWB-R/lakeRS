#' Plot annual dynamics with optional uncertainty intervals
#'
#' Draws one or more 365-day time series and optionally overlays 50% and 95%
#' interval polygons. The function is designed for lake-level or cluster-level
#' dynamics derived from [dynamic_per_pixel()] or [combine_years_dynamic()].
#'
#' @param v_averageList Numeric vector or list of numeric vectors with one value
#'   per day of year.
#' @param v_sdList Optional numeric vector or list of vectors with standard
#'   deviations. If supplied, 50% and 95% intervals are approximated from the
#'   standard deviations.
#' @param df_q50List Optional data frame or list of data frames with lower and
#'   upper 50% interval columns.
#' @param df_q95List Optional data frame or list of data frames with lower and
#'   upper 95% interval columns.
#' @param TScolors Character vector of colors for the time series and polygons.
#' @param TSnames Optional character vector of legend names for the time series.
#' @param df_reference Optional two-column data frame with reference day and
#'   value columns.
#' @param RefName Optional character scalar for the reference-line legend.
#' @param lakeName Character scalar used as plot title prefix.
#' @param ylab Character scalar. Y-axis label.
#' @param ylim Optional numeric vector of length two with y-axis limits.
#'
#' @return No explicit return value. The function draws a base R plot.
#'
#' @details Month separators and labels are drawn for a non-leap 365-day year.
#'   Missing interval values are removed with [rm_na_for_polygon()] before
#'   polygons are drawn.
#'
#' @importFrom grDevices col2rgb rgb
#' @importFrom graphics abline legend lines mtext par plot polygon
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
      text.col = "white")
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

#' Remove missing coordinates before polygon plotting
#'
#' Removes coordinate pairs for which either the x or y value is missing, so that
#' [graphics::polygon()] receives aligned coordinate vectors.
#'
#' @param x_v Numeric vector of x coordinates.
#' @param y_v Numeric vector of y coordinates with the same length as `x_v`.
#'
#' @return A list with elements `x` and `y`, both without coordinates associated
#'   with missing values.
#'
#' @keywords internal
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

