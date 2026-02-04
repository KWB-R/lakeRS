#' Calculate an index per pixel averaged over time
#' 
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param ncImage the data list created by [ndi_per_image()]
#' @param years Numeric vector of all the years to be assessed
#' @param seasonMonths Numeric vector including all months of the growing season
#' @param water_scenes_only By default only pixels classified as water scene are 
#' used for the yearly average per pixel. If False, all values are included no
#' except for cloud and snow scenes.
#' @param pixelQualityThreshold The quality is defined as the minimum proportion
#' a pixel needs to be defined as water scene to be included.
#' @param lakeInfo Character vector of length 2 specifying the Name and the ID
#' of the lake. This is not needed for any calculation but is important to 
#' identify the lake.
#' 
#' @return 
#' A list of 
#'  - lakeInfo: A character of lake Name and ID or whatever is provided as 
#'  meta data#
#'  - A list per season
#'    - RSindex: A Matrix of the whole rechtangle containing the index per pixel 
#'    for the whole season
#'    - QualityThreshold: The minimum proportion a pixel is required to be
#'    identified as water during one season
#'    - minValuesPerPixel: The mininum number of images used for index calculation 
#'    ofone pixel (Quality_threshold x Images_of_Season)
#'    - validPixel: The number of pixels above Quality Threshold
#'  
#' @export
#' 
seasonal_index_per_pixel <- function(
    nc, ncImage, years, seasonMonths = 4:10, 
    water_scenes_only = TRUE, pixelQualityThreshold = 0.8, 
    lakeInfo = c("", "")
){
  output <- list()
  output[["lakeInfo"]] <- lakeInfo
  
  for(year in years){
    imageMonth <- as.numeric(format(nc$t_date, "%m"))
    imageYear <- as.numeric(format(nc$t_date, "%Y"))
    
    timeFilter <- 
      as.character(imageMonth) %in% as.character(seasonMonths) &
      as.character(imageYear) == as.character(year)
    
    indexImages <- ncImage$RSindex[timeFilter]
    sclImage <- ncImage$SCL[timeFilter]
    

    for(i in seq_along(indexImages)){
      indexImages[[i]] <- if(water_scenes_only){
        scl_filter(
          indexImage = indexImages[[i]], 
          sclImage = sclImage[[i]], 
          bands = 6, 
          invert = TRUE)
      } else {
        scl_filter(
          indexImage = indexImages[[i]], 
          sclImage = sclImage[[i]], 
          bands = c(8:11))    
      }
    }
    
    if(water_scenes_only){
      indexImages <- lapply(seq_along(indexImages), function(i){
        indexImages[[i]][sclImage[[i]] != 6] <- NA
        indexImages[[i]]
      })
    } else {
      indexImages <- lapply(seq_along(indexImages), function(i){
        indexImages[[i]][sclImage[[i]] %in% c(8:11)] <- NA
        indexImages[[i]]
      })     
    }
    
    RSindex <- pixel_wise_average(
      list_of_mats = indexImages, 
      na.rm = TRUE
    )
    
    waterLayer <- lakeRS::waterscene_proportion(scl_image = sclImage)

    pixelFilter <- waterLayer$water >= pixelQualityThreshold
    if(sum(pixelFilter) == 0L){
      warning("No pixel meets the required water scene proportion defined as",
      " quality threshold.")
    }
    RSindex[!pixelFilter] <- NA
    
    output[[paste0("y", year)]] <- list(
      "RSindex" = RSindex,
      "QualityThreshold" = pixelQualityThreshold, 
      "minValuesPerPixel" = floor(pixelQualityThreshold *
                                    (1 - median(waterLayer$NoFalseDisturbance)) * 
                                    sum(timeFilter)),
      "validPixel" = sum(pixelFilter)
    )
  }
  output
}

#' NDTri per pixel
#' 
#' @param list_of_mats A list of numeric matrices of the same size
#' @param na.rm Logical
#' 
pixel_wise_average <- function(list_of_mats, na.rm = FALSE){
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
