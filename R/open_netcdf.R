#' Open an openEO netCDF data cube
#'
#' Reads a netCDF file into a `terra` SpatRaster and extracts spatial, temporal,
#' band, and CRS metadata used by the lakeRS processing workflow.
#'
#' @param filePath Character scalar. Directory containing the netCDF file.
#' @param fileName Character scalar. File name including the `.nc` extension.
#'   Default is `"openEO.nc"`.
#'
#' @return A list with `bands`, `x`, `y`, `t`, `t_date`, `crs`, and `SpRast`.
#'   `x` and `y` are pixel-center coordinates, `t` contains numeric time steps,
#'   and `t_date` converts these time steps to dates using origin
#'   `1990-01-01`.
#'
#' @details The function stops if subdatasets in the netCDF file have different
#'   row, column, or layer dimensions.
#'
#' @importFrom terra describe rast xFromCol yFromRow crs
#' @export
#' 
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
