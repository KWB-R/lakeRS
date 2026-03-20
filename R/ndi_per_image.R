#' Normalized Difference Index per Pixel and Image
#' 
#' @param nc The netCDF data list created by [open_netcdf()]
#' @param year Numeric value of year to be assessed.
#' @param bandNames A character vector of length 2 defining the sentinel-2
#' bands. To see what bands are available in the datacube run 'nc$bands'.
#' The default is "B05" and "B02" which is used for NDTrI calculation.
#' @param monthFilter Optional numeric vector of months used for the index. 
#' If NULL all months are used.
#' 
#' @details
#' The Index will be calculated by 
#' NDI = (bandNames_1 - bandNames_2) / (bandNames_1 + bandNames_2)
#' 
#' Negative reflactance values are set to zero. See details of 
#' [load_BandLayer()]
#' 
#' The SCL band on the other hand sometimes contains NA values which are 
#' problematic for further processing. NA values are substituted by 0 which 
#' is no ID for a scene 
#' 
#' @export
#' 
ndi_per_image <- function(
    nc, year, bandNames = c("B05", "B02"), monthFilter = NULL
){
  if(length(year) > 1){
    stop("Index can be calculated only on a yearly basis: please define 1 year only.")
  }
  # for huge data
  # for(t in nc$t){
  #   AN <- paste0(bandNames[1], "_t=", t)
  #   BN <- paste0(bandNames[2], "_t=", t)
  #   IN <- paste0("RSindex_t=", t)
  #   
  #   nc$SpRast[[AN]][nc$SpRast[[AN]] < 0] <- 0
  #   nc$SpRast[[BN]][nc$SpRast[[BN]] < 0] <- 0
  #   
  #   nc$SpRast[[IN]] <- 
  #     (nc$SpRast[[AN]] - nc$SpRast[[BN]]) / (nc$SpRast[[AN]] + nc$SpRast[[BN]])
  # }
  At <- load_BandLayer(
    nc = nc, 
    band = bandNames[1], 
    monthFilter = monthFilter, 
    yearFilter = year
  )
  
  Bt <- load_BandLayer(
    nc = nc, 
    band = bandNames[2],
    monthFilter = monthFilter, 
    yearFilter = year
  )
  
  cat("Calculating Normalized Difference Index ... \n")
  list(
    "x" = At$x,
    "y" = At$y,
    "t" = At$t,
    "t_date" = At$t_date,
    "crs" = At$crs, 
    "RSindex" =  mapply(
      FUN = function(x, y){(x-y)/(x+y)}, 
      At$band, Bt$band, 
      SIMPLIFY = FALSE
    )
  )
}
