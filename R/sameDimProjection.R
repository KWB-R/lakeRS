#' Reproject a raster while preserving row and column counts
#'
#' Reprojects a terra input raster to a specified target coordinate reference 
#' system (CRS), preserving the original number of rows and columns. The spatial 
#' extent of the raster is transformed to the new CRS, and a new raster template 
#' is created to match these dimensions before projection.
#'
#' @param initial_raster A `terra::SpatRaster` object.
#' @param finalCRS Character string specifying the target CRS, in `EPSG` or WKT format. 
#' Default is `"EPSG:4326"`.
#'
#' @return A `SpatRaster` object reprojected to the specified CRS, maintaining the same
#'   number of rows and columns as the input raster.
#'
#' @details The input extent is projected to the target CRS and used to create a
#'   template raster. Projection uses nearest-neighbor resampling (`method =
#'   "near"`), which is appropriate for categorical layers and avoids creating
#'   interpolated class values.
#'
#' @seealso [terra::project()], [terra::rast()], [terra::ext()]
#'
#' @importFrom terra project ext crs rast
#' @export
#' 
sameDimProjection <- function(
    initial_raster, finalCRS = "EPSG:4326"
){
  prFrame <- terra::project(
    x = terra::ext(initial_raster), 
    from = terra::crs(initial_raster), 
    to = finalCRS
  )
  
  tmpl <- terra::rast(
    nrows = nrow(initial_raster),
    ncols = ncol(initial_raster),
    ext = prFrame,
    crs = finalCRS
  )
  
  terra::project(x = initial_raster, y = tmpl, method = "near")
}

