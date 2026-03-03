#' Load values of one Band from netCDF into R environment
#' 
#' @param nc The output of [open_netcdf()] function or an extension of it
#' @param band Character of the sentinel-2 band name
#' @param monthFilter Numeric vector of months to load
#' @param yearFilter Numeric vector of years to load
#' 
#' @details
#' Sometime the reflactance bands show negative values. Those values need to be
#' set to 0 before the index is calculated the same is done in the sentinel hub 
#' to harmonize the data: https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/
#' 
#' @export
#
load_BandLayer <- function(
    nc, band, monthFilter = NULL, yearFilter = NULL
){
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
