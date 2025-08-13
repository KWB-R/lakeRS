#' data cube data is sliced according to timesteps, preprocessed and used to
#' calculate the NDTrI
#' 
#' @param nc The netCDF data list created by [load_netcdf()]
#' 
#' @details
#' Sometime the reflactance bands show negative values. Those values need to be
#' set to 0 before the index is calculated the same is done in the sentinel hub 
#' to harmonize the data: https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/
#' 
#' The SCL band on the other hand sometimes contains NA values which are 
#' problematic for further processing. NA values are substituted by 0 which 
#' is no ID for a scene 
#' 
#' @export
#' 
data_per_image <- function(nc){
  b2_image <- b5_image <- scl_image <- ndtri_image <- list()
  for(t_step in seq_along(nc[["t"]])){
    
    b2t <- b2_image[[t_step]] <- nc[["B02"]][, , t_step] 
    b2t[b2t < 0] <- 0
    b5t <- b5_image[[t_step]] <- nc[["B05"]][, , t_step]
    b5t[b5t < 0] <- 0
    sclt <- nc[["SCL"]][, , t_step]
    sclt[is.na(sclt)] <- 0
    scl_image[[t_step]] <- sclt
    ndtri_image[[t_step]] <- (b5t - b2t) / (b5t + b2t) 
    
    # remove all cloud and snow pixels 
    ndtri_image[[t_step]][sclt %in% c(2,3, 7:11)] <- NA
  }
  
  list("B02" = b2_image,
       "B05" = b5_image,
       "SCL" = scl_image,
       "ndtri" = ndtri_image)
}
