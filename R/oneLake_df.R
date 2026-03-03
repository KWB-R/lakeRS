#' Reshapes NDTrI Lake list to dataframe
#' 
#' @param lakeIndexList A list of yearly lake index lists of one lake created by
#'   [seasonal_index_per_lake()].
#' 
#' @export
#' 
oneLake_df <- function(lakeIndexList){
  output <- lapply(lakeIndexList, function(x){
    data.frame(
      "lakeName" = x$Name,
      "lakeID" = x$ID,
      "year" = x$year,
      "NDTrI_median" = round(x$IndexMedian, 3), 
      "NDTrI_sd" = round(x$IndexSD, 4), 
      "NDTrI_modusBest" = round(x$IndexModusBest, 3),
      "nValidPixel" = x$nValidPixel,
      "QualityThreshold" = x$QualityThreshold)
  })
  do.call(rbind, output)
}

