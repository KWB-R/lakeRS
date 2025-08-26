#' Function loads a netCDF file and imports all the important information for NDTrI
#' 
#' @param filePath the path of the netCDF file
#' @param fileName The file name including .nc ending. By default openEO 
#' provides a file that is named "openEO.nc"
#' @param vars A character defining the variables of the netcdf to be loaded.
#' The default is B02, B05 and SCL which are needed for NDTrI calculation.
#' If "all" is specified, all available variables are loaded.  
#' 
#' @return
#' Returns a list of spatial and temporal variables and all the other layers of 
#' the data cube.
#' 
#' @importFrom ncdump NetCDF
#' @importFrom raster raster crs as.array brick
#' 
#' @export
load_netcdf <- function(
    filePath, fileName = "openEO.nc", vars = c("B02", "B05", "SCL")
){
  ncMeta <- ncdump::NetCDF(x = file.path(filePath, fileName))
  availableVars <- ncMeta$variable$name
  availableVars <- availableVars[availableVars != "crs"]
  if("all" %in% vars){
    vars <- availableVars
  }
  dimDf <- ncMeta$dimension
  x_id <-  dimDf$id[dimDf$name == "x"]
  y_id <-  dimDf$id[dimDf$name == "y"]
  t_id <-  dimDf$id[dimDf$name == "t"]
  
  valDf <- ncMeta$dimension_values
  nc <- list()
  nc[["x"]] <- valDf$vals[valDf$id == x_id]
  nc[["y"]] <- valDf$vals[valDf$id == y_id]
  nc[["t"]] <- valDf$vals[valDf$id == t_id]
  nc[["t_date"]] <- as.Date(nc[["t"]], origin = "1990-01-01")
  
  rasterData <- raster::raster(
    x = file.path(filePath, fileName), 
    varname = availableVars[1])
  crsInfo <- raster::crs(rasterData)
  
  pixel_per_image <- length(nc[["x"]]) * length(nc[["y"]])
  if(pixel_per_image < 4000000){
    for(varName in vars){
      nc[[varName]] <- raster::as.array(
        x = raster::brick(
          x = file.path(filePath, fileName), 
          varname = varName
        )
      )
    }
    nc[["crs"]] <- crsInfo
  } else {
    nc <- NULL
    warning("Number of pixels per image too large.", 
         " Please use function 'index_from_netcdf()' instead.")
  }
  nc
}