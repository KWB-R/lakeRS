#' Aggregation of all pixel NDTrIs into one lake NDTrI
#' 
#' @param ndtriPixels A list of NDTrI per pixel of one or more years, created by
#' [seasonal_index_per_pixel()]
#'
#'
#' @importFrom stats median sd
#' @export
#' 
aggregate_NDTrI <- function(ndtriPixels)
{
  output <- list()
  output[["lakeInfo"]] <- ndtriPixels$lakeInfo
  ndtriPixels <- ndtriPixels[-which(names(ndtriPixels) == "lakeInfo")]
  
  for(N in names(ndtriPixels)){
    output[[N]] <- list()
    
    mat <- ndtriPixels[[N]]$RSindex
    if(ndtriPixels[[N]]$validPixel > 1){
      
      d <- stats::density(
        mat, 
        bw = "SJ", 
        adjust = 2, 
        na.rm = TRUE)
      
      peak_i <- which(diff(sign(diff(d$y))) == -2) 
      modus <- d$x[peak_i[d$y[peak_i] > 1.5 * mean(d$y)]]
      
      best_guess <- 
        if(length(modus) > 0L){
          x_modus <- which(d$x %in% modus)
          modus[order(d$y[x_modus], decreasing = TRUE)[1]]
        } else {
          NA
        }
      
      output[[N]][["ndtriPixels"]] <- mat
      output[[N]][["nValidPixel"]] <- ndtriPixels[[N]]$validPixel
      output[[N]][["QualityThreshold"]] <- ndtriPixels[[N]]$QualityThreshold
      output[[N]][["nValidImages"]] <- ndtriPixels[[N]]$minValuesPerPixel
      output[[N]][["NDTrI_median"]] <- median(mat, na.rm = TRUE)
      output[[N]][["NDTrI_sd"]] <- sd(mat, na.rm = TRUE)
      output[[N]][["NDTrI_modusPot"]] <- modus
      output[[N]][["NDTrI_modusBest"]] <- best_guess
      output[[N]][["density"]] <- d
    } else if(ndtriPixels[[N]]$validPixel == 1L){
      output[[N]][["ndtriPixels"]] <- mat
      output[[N]][["nValidPixel"]] <- ndtriPixels[[N]]$validPixel
      output[[N]][["QualityThreshold"]] <- ndtriPixels[[N]]$QualityThreshold
      output[[N]][["nValidImages"]] <- ndtriPixels[[N]]$minValuesPerPixel
      output[[N]][["NDTrI_median"]] <- median(mat, na.rm = TRUE)
      output[[N]][["NDTrI_sd"]] <- NA
      output[[N]][["NDTrI_modusPot"]] <- median(mat, na.rm = TRUE)
      output[[N]][["NDTrI_modusBest"]] <- median(mat, na.rm = TRUE)
      output[[N]][["density"]] <- NA
    } else {
      output[[N]][["ndtriPixels"]] <- mat
      output[[N]][["nValidPixel"]] <- ndtriPixels[[N]]$validPixel
      output[[N]][["QualityThreshold"]] <- ndtriPixels[[N]]$QualityThreshold
      output[[N]][["nValidImages"]] <- ndtriPixels[[N]]$minValuesPerPixel
      output[[N]][["NDTrI_median"]] <- output[[N]][["NDTrI_sd"]] <-
        output[[N]][["NDTrI_modusPot"]] <- output[[N]][["NDTrI_modusBest"]] <-
        output[[N]][["density"]] <- NA
    }
    
  }
  output
}

