#' Set values NA by SCL
#' 
#' @param indexImage,sclImage Index and SCL layer of the image
#' @param bands Numeric vector specifiyng the SCL categories to filtered for
#' @param invert If TRUE the filtering is inverted -> specefied bands are removed
#' 
scl_filter <- function(indexImage, sclImage, bands, invert = FALSE){
  if(invert){
    indexImage[!(sclImage %in% bands)] <- NA
  } else {
    indexImage[!(sclImage %in% bands)] <- NA
  }
  indexImage
}
