#' Filter net cdf input by date
#' 
#' @param nc The output of [load_netcdf()] function
#' @param tBeg,tEnd Dates used for filtering in the format "YYYY-mm-dd"
#' 
#' @return the same list as nc filtered for the defined time period
#' 
#' @importFrom methods is
#' 
#' @export
#' 
nc_time_filter <- function(nc, tBeg, tEnd){
  imageSelect <- nc$t_date >= as.Date(tBeg) & nc$t_date <= as.Date(tEnd)
  n <- sum(imageSelect)
  if(n > 0){
    ncVars <- names(nc)[
      sapply(nc, function(x){"RasterBrick" %in% is(x)})
    ]
    timeRange <- range(which(imageSelect))
    for(ncVar in ncVars){
       nc[[ncVar]] <- raster::subset(
         x = nc[[ncVar]], 
         subset = timeRange[1]:timeRange[2], 
         drop = FALSE)
       
    }
    nc$t <- nc$t[imageSelect]
    nc$t_date <- nc$t_date[imageSelect]
    return(nc)
  } else {
    stop("No available images in the defined time interval.")
  }
}




