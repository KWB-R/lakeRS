#' Plot the histrom of all pixel NDTrIs of a lake for one year
#' 
#' @param ndtriLake the output of [aggregate_NDTrI()]
#' @param year The year to be plotted as numeric
#' 
#' @importFrom graphics abline hist lines par text
#'
#' @export
#' 
plot_lake_ndtri_histogram <- function(ndtriLake, year){
  lakeName <- ndtriLake$lakeInfo[1]
  lakeID <- ndtriLake$lakeInfo[2]
  y <- ndtriLake[[paste0("y", year)]]
  d <- y$density
  nBreaks <- if(y$nValidPixel > 50){
    y$nValidPixel/10
  } else {
    5
  }
  
  me <- y$NDTrI_median
  mo <- y$NDTrI_modusBest
  hist(y$ndtriPixels, xlab = "NDTrI per Pixel",  
       main = paste0(lakeName, " - ", year),
       breaks = nBreaks, col = "gray80", freq = FALSE, border = NA)
  lines(x = d$x, y = d$y, lwd = 2)
  abline(v = y$NDTrI_modusPot, lty = "dashed")
  abline(v = y$NDTrI_modusBest, lty = "dashed", lwd = 2)
  abline(v = y$NDTrI_median, lty = "dotted", lwd = 2, col = "cadetblue")
  text(x = y$NDTrI_modusBest, labels = paste0("Selected modus\n", round(mo, 3)), 
       y = par("usr")[4], pos = ifelse(mo > me, 4, 2), cex = 0.8, xpd = TRUE)
  text(x = y$NDTrI_median, labels = paste0("Median of pixels\n", round(me, 3)), 
       col = "cadetblue", 
       y = par("usr")[4], pos = ifelse(mo > me, 2, 4), cex = 0.8, xpd = TRUE)
  
}
