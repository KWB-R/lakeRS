#' data cube data is sliced according to timesteps, preprocessed and used to
#' calculate a normalized difference index
#' 
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param bandNames A character vector of length 2 defining the sentinel-2
#' bands. To see what bands are available in the datacube run 'names(nc)'.
#' The default is "B05" and "B02" which is used for NDTrI calculation.
#' @param keep_bands If True the band 2, 5 and SCL data is part of the list (as
#' matrix per image)
#' 
#' @details
#' The Index will be calculated by 
#' NDI = (bandNames_1 - bandNames_2) / (bandNames_1 + bandNames_2)
#' 
#' Negative reflactance values are set to zero. See details of 
#' [load_bandLayer()]
#' 
#' The SCL band on the other hand sometimes contains NA values which are 
#' problematic for further processing. NA values are substituted by 0 which 
#' is no ID for a scene 
#' 
#' @export
#' 
ndi_per_image <- function(
    nc, bandNames = c("B05", "B02"), keep_bands = FALSE
){
 
  bA_image <- bB_image <- scl_image <- list()
  
  ndi_image <- list()
  for(t_step in seq_along(nc[["t"]])){
    print(paste0("processing ", t_step, " / ", length(nc[["t"]])))
    
    bAt <- load_bandLayer(nc = nc, bandName = bandNames[1], timeStep = t_step)
    bBt <- load_bandLayer(nc = nc, bandName = bandNames[2], timeStep = t_step)
    sclt <- load_bandLayer(nc = nc, bandName = "SCL", timeStep = t_step)
    sclt[is.na(sclt)] <- 0
    
    if(keep_bands){
      bA_image[[t_step]] <- bAt
      bB_image[[t_step]] <- bBt
      scl_image[[t_step]] <- sclt
    } 
    
    ndi_image[[t_step]] <- (bAt - bBt) / (bAt + bBt) 
  }
  
  list("B_1" = bA_image,
       "B_2" = bB_image,
       "SCL" = scl_image,
       "RSindex" = ndi_image)
}

#' Data cube data is sliced according to timesteps, preprocessed and used to
#' calculate an additive index
#' 
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param bandNames A character vector of length 2 defining the sentinel-2
#' bands. To see what bands are available in the datacube run 'names(nc)'.
#' The default is "B05" and "B02" which is used for NDTrI calculation.
#' @param keep_bands If True the bandsand SCL data is part of the list (as
#' matrix per image)
#' @param normalFactor The factor that is multiplied to the bands for normalizing
#' purpose before they are used in the rgb function.
#' 
#' @details
#' The Index will be calculated by 
#' NDI = (bandNames_1 - bandNames_2) / (bandNames_1 + bandNames_2)
#' 
#' Negative reflactance values are set to zero. See details of 
#' [load_bandLayer()]
#' 
#' The SCL band on the other hand sometimes contains NA values which are 
#' problematic for further processing. NA values are substituted by 0 which 
#' is no ID for a scene 
#' 
#' @export
#' 
additive_per_image <- function(
    nc, bandNames = c("B02", "B03", "B04"), normalFactor = 1/10000, keep_bands = FALSE
){
  
  bA_image <- bB_image <- bC_image <- scl_image <- addIndex <- list()
  
  for(t_step in seq_along(nc[["t"]])){
    print(paste0("processing ", t_step, " / ", length(nc[["t"]])))
    
    bAt <- load_bandLayer(nc = nc, bandName = bandNames[1], timeStep = t_step)
    bBt <- load_bandLayer(nc = nc, bandName = bandNames[2], timeStep = t_step)
    bCt <- load_bandLayer(nc = nc, bandName = bandNames[3], timeStep = t_step)
    
    sclt <- load_bandLayer(nc = nc, bandName = "SCL", timeStep = t_step)
    sclt[is.na(sclt)] <- 0
    
    if(keep_bands){
      bA_image[[t_step]] <- bAt
      bB_image[[t_step]] <- bBt
      bC_image[[t_step]] <- bCt
      scl_image[[t_step]] <- sclt
    } 
    
    bAt <- bAt * normalFactor
    bBt <- bBt * normalFactor
    bCt <- bCt * normalFactor
    
    bAt[bAt > 0.5] <- 0.5
    bBt[bBt > 0.5] <- 0.5
    bCt[bCt > 0.5] <- 0.5
    
    bAt <- bAt * 2
    bBt <- bBt * 2
    bCt <- bCt * 2
    
    addIndex[[t_step]] <- matrix(
      rgb(red = bAt, green = bBt, blue = bCt), 
      nrow = nrow(bAt), ncol = ncol(bAt)
    ) 
  }
  
  list("B_1" = bA_image,
       "B_2" = bB_image,
       "B_3" = bC_image,
       "SCL" = scl_image,
       "RSindex" = addIndex)
}

#' Load one layer of a band from a datacrube
#' 
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param bandName A character vector of length 2 defining the sentinel-2
#' bands. To see what bands are available in the datacube run 'names(nc)'.
#' @param timeStep Integer of the layer (timestep in a timeseries)
#' 
#' @details
#' Sometime the reflactance bands show negative values. Those values need to be
#' set to 0 before the index is calculated the same is done in the sentinel hub 
#' to harmonize the data: https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/
#' 
#' @importFrom raster as.matrix subset
#' @export
#' 
load_bandLayer <- function(nc, bandName, timeStep){
  output <- raster::as.matrix(
    raster::subset(
      x = nc[[bandName]], 
      subset = timeStep, 
      drop = TRUE
    )) 
  output[output < 0] <- 0 
  output
}

