#' Load one band from an opened netCDF data cube
#'
#' Extracts all time layers of a selected Sentinel-2 band from an [open_netcdf()]
#' object, optionally restricted to selected months and years.
#'
#' @param nc A list returned by [open_netcdf()] or a compatible object containing
#'   `SpRast`, `bands`, `x`, `y`, `t`, `t_date`, and `crs`.
#' @param band Character scalar with the band name to load, for example `"B02"`,
#'   `"B05"`, or `"SCL"`.
#' @param monthFilter Optional numeric vector of months to retain.
#' @param yearFilter Optional numeric vector of years to retain.
#'
#' @return A list containing coordinates, time metadata, CRS, and `band`, a list
#'   of matrices with one matrix per retained time step.
#'
#' @details Negative band values are set to zero before matrices are returned.
#'   This is intended for reflectance bands and follows the  Sentinel-2
#'   pre-processing convention to harmonize data 
#'   (https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/)
#'
#' @export
#' 
load_BandLayer <- function(
    nc, band, monthFilter = NULL, yearFilter = NULL
){
  cat(paste("Loading values of Band", band, "... \n"))
  if(!is.null(monthFilter)){
    mf <- as.integer(format(nc$t_date, "%m")) %in% as.integer(monthFilter)
    if(sum(mf) > 0){
      if(sum(mf) < length(nc$t)){
        t_steps <- nc$t[mf]
        nc$SpRast <- nc$SpRast[[
          grep(
            pattern = paste0("t=", t_steps, collapse =  "|"), 
            x = names(nc$SpRast))
        ]]
        nc$t <- nc$t[mf]
        nc$t_date <- nc$t_date[mf]
      }
    } else {
      stop("No images in months ", paste0(monthFilter, collapse = ", "))
    }
  }
  if(!is.null(yearFilter)){
    yf <- as.integer(format(nc$t_date, "%Y")) %in% as.integer(yearFilter)
    if(sum(yf) > 0){
      if(sum(yf) < length(nc$t)){
        t_steps <- nc$t[yf]
        nc$SpRast <- nc$SpRast[[
          grep(
            pattern = paste0("t=", t_steps, collapse =  "|"), 
            x = names(nc$SpRast))
        ]]
        nc$t <- nc$t[yf]
        nc$t_date <- nc$t_date[yf]
      }
    } else {
      stop("No images in years ", paste0(yearFilter, collapse = ", "))
    }
  }
  allBands <- nc$bands
  band_i <- which(allBands %in% band)
  bandVectors <- nc$SpRast[
    1:length(nc$y), 
    1:length(nc$x), 
    1:length(nc$t) + (band_i - 1) * length(nc$t)
  ]
  bandVectors[bandVectors < 0] <- 0
  list("x" = nc$x,
       "y" = nc$y, 
       "t" = nc$t, 
       "t_date" = nc$t_date, 
       "crs" = nc$crs,
       "band" = lapply(
         bandVectors,
         matrix,  
         nrow = length(nc$y), 
         ncol = length(nc$x), 
         byrow = TRUE
       )
  )
}
