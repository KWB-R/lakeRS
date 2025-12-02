#' Function loads a netCDF file and imports all the important information for NDTrI
#' 
#' @param filePath the path of the netCDF file
#' @param fileName The file name including .nc ending. By default openEO 
#' provides a file that is named "openEO.nc"
#' @param vars A character defining the variables of the netcdf to be loaded.
#' The default is B02, B05 and SCL which are needed for NDTrI calculation.
#' If "all" is specified, all available variables are loaded.  
#' @param start_xy,count_xy The first value and the length of the from netCDF 
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
#' @importFrom raster raster crs brick extend crop
#' 
#' @export
load_netcdf <- function(
    filePath, fileName = "openEO.nc", vars = c("B02", "B05", "SCL"),
    start_xy = c(10,10), count_xy = c(NA,NA)
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
  
  dimDf <- ncMeta$dimension
  to_many_counts <- c(dimDf$len[dimDf$name == "x"], dimDf$len[dimDf$name == "y"]) - 
    (count_xy + start_xy - 1)
  if(any(to_many_counts < 0)){
    count_xy[to_many_counts < 0] <- count_xy[to_many_counts < 0] + to_many_counts[to_many_counts < 0] 
  }
  
  crop_nc <- FALSE
  v_crop <- c(
    "xmin" = start_xy[1], 
    "xmax" = start_xy[1] + count_xy[1] - 1, 
    "ymin" = start_xy[2], 
    "ymax" = start_xy[2] + count_xy[2] - 1
  )
  
  availableVars <- ncMeta$variable$name
  availableVars <- availableVars[availableVars != "crs"]
  if("all" %in% vars){
    vars <- availableVars
  }
  
  if(count_xy[1] < dimDf$len[dimDf$name == "x"] |
     count_xy[2] < dimDf$len[dimDf$name == "y"]){
    crop_nc <- TRUE
  }
  
  x_id <-  dimDf$id[dimDf$name == "x"]
  y_id <-  dimDf$id[dimDf$name == "y"]
  t_id <-  dimDf$id[dimDf$name == "t"]
  
  valDf <- ncMeta$dimension_values
  cat(paste0("Loading Coordinates and time variables ... \n"))
  nc <- list()
  nc[["x"]] <- valDf$vals[valDf$id == x_id]
  nc[["y"]] <- valDf$vals[valDf$id == y_id]
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
      if(crop_nc){
        bb_crop <- c(
          range(nc$x[v_crop[1:2]]),
          range(nc$y[v_crop[3:4]])
        )
        e <- raster::extent(bb_crop)
        nc[[varName]] <- raster::crop(nc[[varName]], e)
      }
  }
    nc[["crs"]] <- crsInfo
  } else {
    nc <- NULL
    warning("Number of pixels per image too large.", 
         " Please use function 'index_from_netcdf()' instead.")
  }
  nc
}