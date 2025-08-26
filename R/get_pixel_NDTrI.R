#' Calculate the Normalized difference trophic index for a year
#' 
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param ncImage the data list created by [ndtri_per_image()]
#' @param years Numeric vector of all the years to be assed
#' @param seasonMonths Numeric vector including all months of the growing season
#' @param water_scenes_only By default only pixels classified as water scene are 
#' used for the yearly average per pixel. If False, all values are included no
#' matter what scene they are
#' @param pixelQualityThreshold The quality is defined as the minimum proportion
#' a pixel needs to be defined as water scene to be included.
#' @param lakeInfo Character vector of length 2 specifying the Name and the ID
#' of the lake. This is not needed for any calculation but is important to 
#' identify the lake.
#' 
#' @export
#' 
get_pixel_NDTrI <- function(
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
    
    ndtriImages <- ncImage$ndtri[timeFilter]
    sclImage <- ncImage$SCL[timeFilter]
    
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
    
    waterLayer <- lakeRS::waterscene_proportion(scl_image = sclImage)
    
    pixelFilter <- waterLayer$water >= pixelQualityThreshold
    ndtri[!pixelFilter] <- NA
    
    output[[paste0("y", year)]] <- list(
      "NDTrI" = ndtri,
      "QualityThreshold" = pixelQualityThreshold, 
      "minValuesPerPixel" = floor(pixelQualityThreshold * sum(timeFilter)),
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
