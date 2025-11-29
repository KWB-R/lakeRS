#' Plot of images per day and year
#' 
#' @param vDate A vector of image dates
#' @param vCoverage A dataframe of scene proportion per image created by 
#' [nc_scene_per_image()]
#' 
#' @importFrom graphics legend mtext
#' @export
#' 
plot_image_dates <- function(vDate, vCoverage = NULL){
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
  par(mar = c(4.1, 4.1, 4.1, 4.1))
  plot(x = 0, y = 0, type = "n", xlim = c(1,366), ylim = c(1, length(y)),
       xlab = "Day of the year", ylab = "", xaxt = "n", yaxt = "n", 
       xaxs = "i")
  
  upperLimits <- c(0.05, 0.1, 0.3, 0.5, 0.75, 1)
  usedColors <- c("green3", "lightgreen", "yellow", "orange", "red", "darkred")
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
      points(x = x$dayOfYear[j], 
             y = length(yearNumbers) + 1 - i + yPos, 
             pch = "|", 
             cex = 2, col = as.character(x$col[j]))
    }
  }
  plotVertical <- par("usr")[3:4]
  plotVertical_s <- par("plt")[3:4]
  plotTop <- plotVertical[2]
  marginTop <- plotTop +
    diff(plotVertical) / diff(plotVertical_s) * (1 - plotVertical_s [2])
  legend(x = 366/2, y = mean(c(plotTop, marginTop)), xjust = 0.5, yjust = 0.5, 
         legend = c("<5%", "5-10%", "10-30%", "30-50%", "50-75%", ">75%"), 
         pch = "|", pt.cex = 2, col = usedColors,
         bty = "n", horiz = TRUE, 
         title = "Images per day", xpd = TRUE)
  mtext(text = "Images per Year", side = 4, line = 3)
  axis(4, at = length(yearNumbers):1, sapply(y, nrow), las = 2, tick = FALSE)
}
