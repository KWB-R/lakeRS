#' Open netCDF file
#' 
#' @param filePath the path of the netCDF file
#' @param fileName The file name including .nc ending. By default openEO 
#' provides a file that is named "openEO.nc"
#' 
#' @return
#' Returns a list of spatial and temporal variables, the netcdf CRS (coordinate 
#' reference system) and the spatRaster of all layers
#' 
#' @importFrom terra describe rast xFromCol yFromRow crs
#' 
#' @export
open_netcdf <- function(
    filePath, fileName = "openEO.nc"
){
  ncMeta <- terra::describe(
    x = file.path(filePath, fileName), sds = TRUE)
  xLen <- unique(ncMeta$ncol)
  yLen <- unique(ncMeta$nrow)
  tLen <- unique(ncMeta$nlyr)
  if(length(xLen) > 1 | length(yLen) > 1 | length(tLen) > 1){
    stop("Different dimensions of band layers in data cube.")
  }
  
  rasterData <- terra::rast(
    x = file.path(filePath, fileName), 
    drivers = "NETCDF"
  )
  
  nc <- list()
  nc[["bands"]] <- ncMeta$var
  nc[["x"]] <- terra::xFromCol(object = rasterData) # center of the pixel
  nc[["y"]] <- terra::yFromRow(object = rasterData) # center of the pixel
  nc[["t"]] <- as.numeric(sapply(names(rasterData)[1:tLen], function(x){
    strsplit(x = x, split = "t=")[[1]][2]
  }))
  nc[["t_date"]] <- as.Date(nc[["t"]], origin = "1990-01-01")
  nc[["crs"]] <- terra::crs(x = rasterData)
  nc[["SpRast"]] <- rasterData
  nc
}
