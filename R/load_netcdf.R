#' Function loads a netCDF file and imports all the important information for NDTrI
#' 
#' @param title Character string. The same title that has been used to start the
#' job by function [start_openEO_job()].
#' @param path the path of the netCDF file
#' @param fileName The file name including .nc ending. By default openEO 
#' provides a file that is named "openEO.nc"
#' @param loadAllVars If TRUE (default) all available variable layers will be 
#' loaded into the output list. Otherwise only bands relevant for the NDTrI will
#' be loaded.
#' 
#' @return
#' Returns a list of spatial and temporal variables and all the other layers of 
#' the data cube.
#' 
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' 
#' @export
load_netcdf <- function(
    title, path, fileName = "openEO.nc", loadAllVars = TRUE
){
  # load and process data
  nc_data <- ncdf4::nc_open(filename = file.path(path, title,  fileName))  
  
  nc <- list()
  # x, y and t
  nc[["x"]] <- ncdf4::ncvar_get(nc = nc_data, varid = "x")
  nc[["y"]] <- ncdf4::ncvar_get(nc = nc_data, varid = "y")
  nc[["t"]] <- ncdf4::ncvar_get(nc = nc_data, varid = "t")
  nc[["t_date"]] <- as.Date(nc[["t"]], origin = "1990-01-01")
  
  vars <- if(loadAllVars){
    grep(
      pattern = "crs", 
      x = names(nc_data$var), 
      value = TRUE, 
      invert = TRUE
    )
  } else {
    c("B02", "B05", "SCL")
  }
  for(var_i in vars){
    nc[[var_i]] <- ncdf4::ncvar_get(nc = nc_data, varid = var_i)
  }
  
  ncdf4::nc_close(nc = nc_data)
  
  nc
}