#' Spread the NDTrI database by year
#' 
#' @param df The database dataframe
#' @param aggregationType Either "modus" (default) or "median"
#' @param filterPixels Integer: Filter for lake trophic state assessment based 
#' on an equal or greater number of "water" pixels per lake. Water pixels are
#' pixels that fullfill the quality threshold value.
#' @param filterImages Integer: Filter for lake trophic state assessment based on an 
#' equal or greater number of images per season 
#' @param filterQuality Number between 0 and 1: Filter for lake trophic state 
#' assessment based on an equal or greater quality threshold (proportion of 
#' pixels that are identified as water by the SCL band)
#' 
#' @return A data frame with columns lakeName, lakeID and one column per year
#' that lists the NDTrI of the lake --> One row per lake
#' 
#' @importFrom tidyr spread
#' @export
#' 
ndtri_spread <- function(
    df, aggregationType = "modus", filterPixels = 0, filterImages = 0, 
    filterQuality = 0
){
  
  df <- df[df$nValidPixel > filterPixels,]
  df <- df[df$nValidImages > filterImages,]
  df <- df[df$QualityThreshold > filterQuality,]
  # Maybe add a quality filter for modus < median --> if modus is higher than median,
  # the majority of pixels comes probably from lake side data --> important for
  # small lakes
  
  typeColumn <- 
  if(aggregationType == "modus"){
    "NDTrI_modusBest"
  } else if(aggregationType == "median"){
    "NDTrI_median"
  }
  
  output <- tidyr::spread(
    data = df[,c("lakeName", "lakeID", "year", typeColumn)], 
    key = c("year"), 
    value = typeColumn)
  
  year_columns <- colnames(output) %in% df$year
  colnames(output)[year_columns] <- paste0("year_", colnames(output)[year_columns])
  output
}