#' Calculate a seasonal index value for each pixel
#'
#' Filters image-wise index matrices by Sentinel-2 SCL information, averages the
#' remaining index values pixel by pixel, and removes pixels that do not meet a
#' water-scene quality threshold.
#'
#' @param imageIndex A list created by [ndi_per_image()] containing image-wise
#'   `RSindex` matrices, time information, coordinates, and CRS.
#' @param nc A netCDF list returned by [open_netcdf()]. Used to load the SCL band
#'   for the same year and months as `imageIndex`.
#' @param water_scenes_only Logical. If `TRUE`, processing keeps SCL class 6
#'   (water) before averaging. If `FALSE`, the current code calls [scl_mask()]
#'   with SCL classes 8 to 11; because `scl_filter()` currently ignores `invert`,
#'   this mode should be checked before use.
#' @param pixelQualityThreshold Numeric between 0 and 1. Minimum water-scene
#'   proportion required for a pixel to be retained in the seasonal index.
#'
#' @return A list with `x`, `y`, `crs`, `year`, `QualityThreshold`, `RSindex`, and
#'   `sceneProportions`. `RSindex` is a matrix of seasonal pixel index values;
#'   pixels below the water-quality threshold are set to `NA`.
#'
#' @details Water-scene proportions are calculated by [waterscene_proportion()].
#'   A warning is issued if no pixel meets `pixelQualityThreshold`.
#'
#' @export
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
  for(i in seq_along(sclt$band)){
    sclt$band[[i]][is.na(sclt$band[[i]])] <- 0
  }
  
  if(!all(imageIndex$t == sclt$t)){
    stop("Something went wrong during timestamp filtering.")
  }
  
  imageIndex$RSindex <-
    if(water_scenes_only){
      mapply(
        FUN = scl_mask, 
        imageIndex$RSindex, 
        sclt$band, 
        sclCategories = 6, invert = TRUE, 
        SIMPLIFY = FALSE
      )
    } else {
      mapply(
        FUN = scl_mask, 
        imageIndex$RSindex, 
        sclt$band, 
        sclCategories = c(8:11), invert = FALSE, 
        SIMPLIFY = FALSE
      )
    }
  
  RSindex <- pixel_wise_average(
    list_of_mats = imageIndex$RSindex, 
    na.rm = TRUE
  )
  
  waterLayer <- lakeRS::waterscene_proportion(scl_image = sclt$band)
  
  pixelFilter <- waterLayer$water >= pixelQualityThreshold
  if(!any(pixelFilter != 0L)){
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


#' Average a list of matrices pixel by pixel
#'
#' Calculates the element-wise average of equally sized matrices.
#'
#' @param list_of_mats A list of numeric matrices with identical dimensions.
#' @param na.rm Logical. If `TRUE`, missing values are ignored by replacing them
#'   with zero and dividing by the number of non-missing values at each pixel.
#'
#' @return A numeric matrix with the same dimensions as the input matrices.
#'
#' @details If `na.rm = TRUE` and a pixel is missing in all matrices, division by
#'   zero can produce `NaN`.
#'
#' @keywords internal
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
