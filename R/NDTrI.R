#' Calculate the Normalized difference trophic index for a year
#' 
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param ncImage the data list created by [data_per_image()]
#' @param waterLayer The water scene proportion layer as part of the SCL list
#' created by [waterscene_proportion()]
#' @param year Numeric value of the year
#' @param seasonMonths Numeric value of length two defining the first and last
#' month (both included) of the growing season.
#' @param water_scenes_only By default only pixels classified as water scene are 
#' used for the yearly average per pixel. If False, all values are included no
#' matter what scene they are
#' @param pixelQualityThreshold The quality is defined as the minimum proportion
#' a pixel needs to be defined as water scene to be included.
#' 
#' @export
#' 
NDTrI <- function(
    nc, ncImage, waterLayer, year, seasonMonths = c(4,10), 
    water_scenes_only = TRUE, pixelQualityThreshold = 0.8
){
  
  imageMonth <- as.numeric(format(nc$t_date, "%m"))
  imageYear <- as.numeric(format(nc$t_date, "%Y"))
  
  timeFilter <- 
    imageMonth >= seasonMonths[1] & 
    imageMonth <= seasonMonths[2] & 
    imageYear == year
  
  ndtriImages <- ncImage$ndtri[timeFilter]
  sclImage <- ncImage$SCL[timeFilter]
  
  # Proportion of all scenes per pixel
  if(water_scenes_only){
    ndtriImages <- lapply(seq_along(ndtriImages), function(i){
      ndtriImages[[i]][sclImage[[i]] != 6] <- NA
      ndtriImages[[i]]
    })
  }
  
  ndtri <- index_wise_average(
    list_of_mats = ndtriImages, 
    na.rm = TRUE
  )
  
  # remove all uncertain pixels
  pixelFilter <- waterLayer >= pixelQualityThreshold
  # if(sum(use_pixel) < 5){
  #   pixel_quality_threshold <- 0.3
  #   use_pixel <- water_proportion >= pixel_quality_threshold
  # }
  ndtri[!pixelFilter] <- NA
  
  list("NDTrI" = ndtri,
       "QualityThreshold" = pixelQualityThreshold, 
       "minValuesPerPixel" = floor(pixelQualityThreshold * sum(timeFilter))
  )
}


#' NDTri per pixel
#' 
#' @param list_of_mats A list of numeric matrices of the same size
#' @param na.rm Logical
#' 
index_wise_average <- function(list_of_mats, na.rm = FALSE){
  if(na.rm){
    no_na_values <- 
      length(list_of_mats) - Reduce("+",lapply(list_of_mats, is.na))
    list_of_mats <- lapply(list_of_mats, function(x){
      x[is.na(x)] <- 0
      x
    })
  } else {
    no_na_values <- length(list_of_mats)
  }
  
  Reduce("+", list_of_mats) / no_na_values
}
