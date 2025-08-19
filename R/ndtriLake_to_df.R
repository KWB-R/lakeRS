#' Reshapes NDTrI Lake list to dataframe
#' 
#' @param ndtriLake A list of NDTrI per pixel of one or more years, created by
#' [aggregate_NDTrI()]
#' 
#' @export
#' 
ndtriLake_to_df <- function(ndtriLake){
  yearlyValues <- grep(pattern = "^y", names(ndtriLake), value = TRUE)
  output <- lapply(yearlyValues, function(N){
    x <- ndtriLake[[N]]
    data.frame(
      "lakeName" = ndtriLake$lakeInfo[1],
      "lakeID" = ndtriLake$lakeInfo[2],
      "year" = as.numeric(gsub(pattern = "y", replacement = "", x = N)),
      "NDTrI_median" = round(x$NDTrI_median, 3), 
      "NDTrI_sd" = round(x$NDTrI_sd, 4), 
      "NDTrI_modusBest" = round(x$NDTrI_modusBest, 3),
      "nValidPixel" = x$nValidPixel,
      "QualityThreshold" = x$QualityThreshold, 
      "nValidImages" = x$nValidImages)
  })
  do.call(rbind, output)
}
