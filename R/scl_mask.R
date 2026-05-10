#' Mask index values by Sentinel-2 SCL classes
#' 
#' Sets index pixels to `NA` based on matching Sentinel-2 scene-classification
#' values.
#' 
#' @param indexImage Numeric matrix of index values.
#' @param sclImage Matrix of SCL values with the same dimensions as `indexImage`.
#' @param sclCategories Numeric vector of SCL classes to keep in the current
#'   implementation.
#' @param invert If TRUE the filtering is inverted. All categories except the
#' specified ones are removed
#' 
#' @keywords internal
#' 
#
scl_mask <- function(indexImage, sclImage, sclCategories, invert = FALSE){
  if(invert){
    indexImage[!(sclImage %in% sclCategories)] <- NA
  } else {
    indexImage[(sclImage %in% sclCategories)] <- NA
  }
  indexImage
}
