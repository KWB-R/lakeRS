#' Create a matrix of clusters in the same dimension as Datacube nc
#'
#' @param clusterVector A named vector of clusters per pixel. The names are 
#'   are made of the index within the datacube xy-matrix combined the "p_" as 
#'   prefix (Vector is part of the returned output by [pixel_clusters()])
#' @param nc A netCDF list returned by [open_netcdf()]. The SCL band is loaded
#'   from this object for scene filtering.
#'   
#' @export
#'   
get_clusterLayer <- function(clusterVector, nc){
  clusterLayer <- matrix(
    data = NA, 
    nrow = length(nc$y), 
    ncol = length(nc$x)
  )
  
  mat_i <- as.numeric(
    gsub(
      pattern = "p_", 
      replacement = "", 
      names(clusterVector)
    )
  )
  clusterLayer[mat_i] <- clusterVector
  clusterLayer
}
