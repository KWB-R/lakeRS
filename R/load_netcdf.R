#' Function loads a netCDF file and imports all the important information for NDTrI
#' 
#' @param filePath the path of the netCDF file
#' @param fileName The file name including .nc ending. By default openEO 
#' provides a file that is named "openEO.nc"
#' @param vars A character defining the variables of the netcdf to be loaded.
#' The default is B02, B05 and SCL which are needed for NDTrI calculation.
#' If "all" is specified, all available variables are loaded.  
#' @start_xy,count_xy The first value and the length of the from netCDF 
#' extracted location values in x and y direciton. If count_xy is c(NA,NA), all 
#' values are loaded. If it is NA for only one axis all values in this direction 
#' are loaded. These arguments can be used for very large files, where
#' the netcdf need to be cut into smaller pieces.
#' 
#' @return
#' Returns a list of spatial and temporal variables and all the other layers of 
#' the data cube.
#' 
#' @importFrom ncdump NetCDF
#' @importFrom raster raster crs brick
#' 
#' @export
load_netcdf <- function(
    filePath, fileName = "openEO.nc", vars = c("B02", "B05", "SCL"),
    start_xy = c(1,1), count_xy = c(NA,NA)
){
  ncMeta <- ncdump::NetCDF(x = file.path(filePath, fileName))
  if(any(is.na(count_xy))){
    all_values <- c("x", "y")[which(is.na(count_xy))]
    if("x" %in% all_values){
      count_xy[1] <- ncMeta$dimension$len[ncMeta$dimension$name == "x"]
    }
    if("y" %in% all_values){
      count_xy[2] <- ncMeta$dimension$len[ncMeta$dimension$name == "y"]
    }
  }
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
  cat(paste0("Loading Coordinates and time variables ... \n"))
  nc <- list()
  nc[["x"]] <- valDf$vals[valDf$id == x_id][start_xy[1]:(start_xy + count_xy - 1)[1]]
  nc[["y"]] <- valDf$vals[valDf$id == y_id][start_xy[2]:(start_xy + count_xy - 1)[2]]
  nc[["t"]] <- valDf$vals[valDf$id == t_id]
  nc[["t_date"]] <- as.Date(nc[["t"]], origin = "1990-01-01")
  
  
  cat("Read CRS information. \n")
  rasterData <- raster::raster(
    x = file.path(filePath, fileName), 
    varname = vars[1], ncols = 1, nrows = 1)
  crsInfo <- raster::crs(rasterData)
  
  
  pixel_per_image <- length(nc[["x"]]) * length(nc[["y"]])
  if(pixel_per_image < 4000000){
    for(varName in vars){
      cat(paste0("Loading ", varName, "... \n"))
      nc[[varName]] <- raster::brick(
          x = file.path(filePath, fileName), 
          varname = varName
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