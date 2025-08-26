#' Identifies lake with highest trends, good or bad
#' 
#' @param numericAssessment The numeric assessment list created by 
#' [EO_assessment_numeric]
#' @param trendPeriod Either "short" or "long"
#' @param trendType Either "better" or "worse" for good or bad trends, 
#' respectively
#' @param outputVector One of "lakeID" (default), "lakeName" or "rowNumber"
#' 
#' @return Ordered vector according to the trend of lake names, lake IDs or
#' rownumbers
#' 
#' @export
#' 
find_trends <- function(numericAssessment, trendPeriod, trendType = "better", outputVector = "lakeID"){
  df <- numericAssessment$assessment
  t <- df[[paste0("trend_", trendPeriod)]]
  trend_available <- which(!is.na(t))
  tOrder <- order(t)
  tOrder <- tOrder[tOrder %in% trend_available]
  if(trendType == "worse"){
    tOrder <- rev(tOrder)
  }
  if(outputVector == "rowNumber"){
    tOrder
  } else {
    df[[outputVector]][tOrder]
  }
}