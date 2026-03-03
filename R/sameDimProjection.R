#' Project a raster to a new CRS while preserving its row and column dimensions
#'
#' This function reprojects an input raster to a specified target coordinate reference system (CRS),
#' preserving the original number of rows and columns. The spatial extent of the raster is transformed
#' to the new CRS, and a new raster template is created to match these dimensions before projection.
#'
#' @param initial_raster A `SpatRaster` object to be projected.
#' @param finalCRS Character string specifying the target CRS, in `EPSG` or WKT format. 
#' Default is `"EPSG:4326"`.
#'
#' @return A `SpatRaster` object reprojected to the specified CRS, maintaining the same
#' number of rows and columns as the input raster.
#'
#' @details
#' The function first projects the spatial extent of the input raster to the target CRS,
#' then constructs a raster template with matching dimensions. The projection uses nearest-neighbor
#' resampling (`method = "near"`). This can be useful for categorical or discrete data where
#' interpolation should be avoided.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' r <- rast(system.file("ex/elev.tif", package = "terra"))
#' r_proj <- sameDimProjection(r, finalCRS = "EPSG:3857")
#' plot(r_proj)
#' }
#'
#' @seealso [terra::project()], [terra::rast()], [terra::ext()]
#'
#' @importFrom terra project ext crs rast
#' @export
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

