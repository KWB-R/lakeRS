#' Plot the distribution of pixel index values for one lake-year
#'
#' Draws a histogram and density curve of all valid pixel-level index values for
#' a lake in one year. The selected modal value and median are highlighted.
#'
#' @param lakeIndex A list created by [seasonal_index_per_lake()].
#' @param lakeName Character scalar used in the plot title.
#' @param indexName Character scalar used as x-axis label.
#'
#' @return `NULL` if no valid pixels are available; otherwise no explicit return
#'   value. The function draws a base R plot.
#'
#' @details The current implementation reads `lakeIndex$nValidePixel`, matching
#'   the current spelling in [seasonal_index_per_lake()].
#'
#' @importFrom graphics abline hist lines par text
#' @export
#' 
plot_lake_index_histogram <- function(
    lakeIndex, lakeName = "", indexName = ""
){
  year <- lakeIndex$year
  vp <- lakeIndex$nValidPixel
 
  if(vp == 0L){
    print("No valid pixel values for ", lakeName, " in ", year)
    return(NULL)
  } else {
    nBreaks <- if(vp > 50){
      vp/10
    } else {
      5
    }
    
    d <- lakeIndex$IndexDensity
    me <- lakeIndex$IndexMedian
    mo <- lakeIndex$IndexModusBest
    
    hist(lakeIndex$IndexPixel, xlab = paste(indexName, "per Pixel"),  
         main = paste0(lakeName, " - ", year),
         breaks = nBreaks, col = "gray80", freq = FALSE, border = NA)
    lines(x = d$x, y = d$y, lwd = 2)
    abline(v = lakeIndex$IndexModusPot, lty = "dashed")
    abline(v = mo, lty = "dashed", lwd = 2)
    abline(v = me, lty = "dotted", lwd = 2, col = "cadetblue")
    text(x = mo, labels = paste0("Selected modus\n", round(mo, 3)), 
         y = par("usr")[4], pos = ifelse(mo > me, 4, 2), cex = 0.8, xpd = TRUE)
    text(x = me, labels = paste0("Median of pixels\n", round(me, 3)), 
         col = "cadetblue", 
         y = par("usr")[4], pos = ifelse(mo > me, 2, 4), cex = 0.8, xpd = TRUE)
    
  }
}
