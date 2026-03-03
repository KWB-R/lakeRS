#' Calculate an index per pixel averaged over time
#' 
#' @param imageIndex the data list created by [ndi_per_image()]
#' @param nc The netcdf list returned by [open_netcdf()]
#' @param water_scenes_only By default only pixels classified as water scene are 
#' used for the yearly average per pixel. If False, all values are included no
#' except for cloud and snow scenes.
#' @param pixelQualityThreshold The quality is defined as the minimum proportion
#' a pixel needs to be defined as water scene to be included.
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
    imageIndex, nc, water_scenes_only = TRUE, pixelQualityThreshold = 0.8
){
  output <- list()
  #output[["lakeInfo"]] <- lakeInfo
  imageMonths <- unique(as.numeric(format(imageIndex$t_date, "%m")))
  imageYear <- unique(as.numeric(format(imageIndex$t_date, "%Y")))
  
  cat("Loading values of SCL Band ... \n")
  sclt <- load_BandLayer(
    nc = nc, 
    band = "SCL", 
    monthFilter = imageMonths, 
    yearFilter = imageYear
  )
  sclt$band[is.na(sclt$band)] <- 0
  if(!all(imageIndex$t == sclt$t)){
    stop("Something went wrong during timestamp filtering.")
  }
  
  imageIndex$RSindex <-
    if(water_scenes_only){
      mapply(
        FUN = scl_filter, 
        imageIndex$RSindex, 
        sclt$band, 
        bands = 6, invert = TRUE, 
        SIMPLIFY = FALSE
      )
    } else {
      mapply(
        FUN = scl_filter, 
        imageIndex$RSindex, 
        sclt$band, 
        bands = c(8:11), invert = FALSE, 
        SIMPLIFY = FALSE
      )
    }
  
  RSindex <- pixel_wise_average(
    list_of_mats = imageIndex$RSindex, 
    na.rm = TRUE
  )
  
  waterLayer <- lakeRS::waterscene_proportion(scl_image = sclt$band)
  
  pixelFilter <- waterLayer$water >= pixelQualityThreshold
  if(sum(pixelFilter) == 0L){
    warning("No pixel meets the required water scene proportion defined as",
            " quality threshold.")
  }
  RSindex[!pixelFilter] <- NA
  
  list(
    "x" = imageIndex$x,
    "y" = imageIndex$y,
    "crs" = imageIndex$crs,
    "year" = imageYear,
    "QualityThreshold" = pixelQualityThreshold, 
    "RSindex" = RSindex,
    "sceneProportions" = waterLayer
  )
}

#' Index per pixel
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
