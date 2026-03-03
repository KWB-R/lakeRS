#' Identifies lake with highest trends, good or bad
#' 
#' @param numericAssessment The numeric assessment list created by 
#' [numericAssessment()]
#' @param trendType Either "short" or "long"
#' @param decreasing logical, sort order increasing or decreasing (default)?
#' @param outputVector One of "lakeID" (default), "lakeName" or "rowNumber"
#' 
#' @return Ordered vector according to the trend of lake names, lake IDs or
#' rownumbers
#' 
#' @export
#' 
find_trends <- function(
    numericAssessment, trendType, decreasing = FALSE, outputVector = "lakeID"
){
  df <- numericAssessment$assessment
  t <- df[[paste0("trend_", trendType)]]
  trend_available <- which(!is.na(t))
  tOrder <- order(t)
  tOrder <- tOrder[tOrder %in% trend_available]
  if(!decreasing){
    tOrder <- rev(tOrder)
  }
  if(outputVector == "rowNumber"){
    tOrder
  } else {
    df[[outputVector]][tOrder]
  }
}