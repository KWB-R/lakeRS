#' Rank lakes by short- or long-term trend
#'
#' Orders lakes or rows from a [numericAssessment()] result according to the
#' selected trend column.
#'
#' @param numericAssessment A list created by [numericAssessment()].
#' @param trendType Character scalar, usually `"short"` or `"long"`, selecting
#'   the column `trend_<trendType>`.
#' @param bestTrendFirst Logical. Passed to the ordering step. A good Trend is
#'   a negative trend, indicating a decrease in eutrophication.
#' @param outputVector Character scalar. One of `"lakeID"`, `"lakeName"`, or
#'   `"rowNumber"`. Determines what is returned.
#'
#' @return An ordered vector of lake IDs, lake names, or row numbers. Rows with
#'   missing trend values are removed from the ranking.
#'
#' @export
#' 
find_trends <- function(
    numericAssessment, trendType, bestTrendFirst = FALSE, outputVector = "lakeID"
){
  df <- numericAssessment$assessment
  t <- df[[paste0("trend_", trendType)]]
  trend_available <- which(!is.na(t))
  tOrder <- order(t)
  tOrder <- tOrder[tOrder %in% trend_available]
  if(!bestTrendFirst){
    tOrder <- rev(tOrder)
  }
  if(outputVector == "rowNumber"){
    tOrder
  } else {
    df[[outputVector]][tOrder]
  }
}