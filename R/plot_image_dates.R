#' Plot of images per day and year
#' 
#' @param vDate A vector of image dates
#' @param vCoverage A dataframe of scene proportion per image created by 
#' [nc_scene_per_image()]
#' @param upperLimits Vector of Numerical values between 0 and 1 that are used
#' as upper limits of coverage intervals. The number of intervals is flexible.
#' 
#' @importFrom graphics legend mtext
#' @importFrom grDevices colorRampPalette
#' @export
#' 
plot_image_dates <- function(
    vDate, vCoverage = NULL, upperLimits = c(0.05, 0.1, 0.3, 0.5, 0.75, 1)
){
 
  
  df <- data.frame("date" = vDate)
  if(!is.null(vCoverage)){
    df <- cbind(df, data.frame("coverage" = vCoverage))
  } else {
    df$coverage <- NA
  }

  df$year <- as.numeric(format(df$date, "%Y"))
  yearNumbers <- unique(df$year)
  df$dayOfYear <- as.numeric(format(df$date, "%j"))
  y <- split(x = df, f = df$year)
  
  legendLimits <- upperLimits[-length(upperLimits)] * 100
  legendText <- c(
    paste0("\u2264 ", legendLimits[1], "%"), 
    unlist(lapply(seq_along(legendLimits)[-1], function(i){
      paste0(legendLimits[i-1], " - ", legendLimits[i], "%")
    })),
    paste0("> ", legendLimits[length(legendLimits)], "%")
  )
    
    paste0(legendLimits[2], " - ", legendLimits[3], "%")
  usedColors <- colorRampPalette(
    c("darkolivegreen1", "chocolate1", "dodgerblue2", "darkmagenta"))(length(upperLimits))
  
 
  par(mar = c(4.1, 4.1, 4.1, 4.1))
  plot(x = 0, y = 0, type = "n", 
       xlim = c(1-0.5,366-0.5), xaxs = "i",
       ylim = c(1-0.5, length(y)+0.5), yaxs = "i",
       xlab = "Day of the year", ylab = "", xaxt = "n", yaxt = "n", 
       xaxs = "i")
  axis(side = 1, at = 1:366, labels = 1:366)
  axis(side = 2, at = length(yearNumbers):1, yearNumbers, las = 2)
  for(i in seq_along(y)){
    x <- y[[i]]
    x$dup <- duplicated(x$dayOfYear)
    x$col <- cut(x$coverage, breaks = c(0, upperLimits), 
        include.lowest = TRUE, 
        labels = usedColors)
    yPos <- 0
    for(j in 1:nrow(x)){
      if(x$dup[j]){
        yPos <- yPos + 0.25
      } else {
        yPos <- 0
      }
      rect(xleft = x$dayOfYear[j] - 0.7, xright = x$dayOfYear[j] + 0.7, 
           ybottom = length(yearNumbers) + 0.7 - i + yPos, 
           ytop = length(yearNumbers) + 1.3 - i + yPos, 
           col =   as.character(x$col[j]), border = NA)
    }
  }
  plotVertical <- par("usr")[3:4]
  plotVertical_s <- par("plt")[3:4]
  plotTop <- plotVertical[2]
  marginTop <- plotTop +
    diff(plotVertical) / diff(plotVertical_s) * (1 - plotVertical_s [2])
  legend(x = 366/2, y = mean(c(plotTop, marginTop)), xjust = 0.5, yjust = 0.5, 
         legend = legendText, lwd = 3, col = usedColors,
         bty = "n", horiz = TRUE, title = "Coverage per Image", xpd = TRUE)
  mtext(text = "Images per Year", side = 4, line = 3)
  axis(4, at = length(yearNumbers):1, sapply(y, nrow), las = 2, tick = FALSE)
}
