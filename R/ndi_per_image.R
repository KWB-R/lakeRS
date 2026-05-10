#' Calculate a normalized difference index for each image and pixel
#'
#' Loads two bands from a netCDF data cube and calculates an pixel-wise
#' normalized difference index for each retained time step.
#'
#' @param nc A netCDF list created by [open_netcdf()].
#' @param year Numeric scalar. Year to process. Only one year can be processed at
#'   a time.
#' @param bandNames Character vector of length two. The index is calculated as
#'   `(bandNames[1] - bandNames[2]) / (bandNames[1] + bandNames[2])`. The default
#'   `c("B05", "B02")` corresponds to the NDTrI setup used by this package.
#' @param monthFilter Optional numeric vector of months to include. If `NULL`,
#'   all months in `year` are used.
#'
#' @return A list containing `x`, `y`, `t`, `t_date`, `crs`, and `RSindex`. The
#'   `RSindex` element is a list of matrices, one per image.
#'
#' @details Band values are loaded through [load_BandLayer()], which sets
#'   negative values to zero before index calculation.
#'
#' @export
#' 
ndi_per_image <- function(
    nc, year, bandNames = c("B05", "B02"), monthFilter = NULL
){
  if(length(year) > 1){
    stop("Index can be calculated only on a yearly basis: please define 1 year only.")
  }
  
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
