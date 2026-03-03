#' Plot the histrom of all pixel NDTrIs of a lake for one year
#' 
#' @param lakeIndex the output of [seasonal_index_per_lake()]
#' @param lakeName,indexName Character strings of the lake and index name used
#' in the plot
#' 
#' @importFrom graphics abline hist lines par text
#'
#' @export
#' 
plot_lake_index_histogram <- function(
    lakeIndex, lakeName, indexName
){
  year <- lakeIndex$year
  vp <- lakeIndex$nValidePixel
 
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
