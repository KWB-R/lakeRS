#' Convert annual lake index results for one lake to a data frame
#'
#' Combines a list of annual [seasonal_index_per_lake()] outputs for one lake
#' into a long-format summary table.
#'
#' @param lakeIndexList A list of yearly lake index lists, usually all belonging
#'   to the same lake.
#'
#' @return A `data.frame` with lake name, lake ID, year, median index, standard
#'   deviation, selected modal index, number of valid pixels, and quality
#'   threshold.
#'
#' @export
#' 
oneLake_df <- function(lakeIndexList){
  output <- lapply(lakeIndexList, function(x){
    data.frame(
      "lakeName" = x$Name,
      "lakeID" = x$ID,
      "year" = x$year,
      "index_median" = round(x$IndexMedian, 3), 
      "index_sd" = round(x$IndexSD, 4), 
      "index_modusBest" = round(x$IndexModusBest, 3),
      "nValidPixel" = x$nValidPixel,
      "QualityThreshold" = x$QualityThreshold)
  })
  do.call(rbind, output)
}

