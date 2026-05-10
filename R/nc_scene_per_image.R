#' Calculate scene-class coverage per image
#'
#' Calculates the proportion of pixels belonging to a selected Sentinel-2 SCL
#' scene group for each image in a netCDF data cube.
#'
#' @param nc A list returned by [open_netcdf()].
#' @param scene Character scalar. One of `"clouds"`, `"water"`, `"snow"`, or
#'   `"shadows"`.
#'
#' @return A `data.frame` with columns `date` and `coverage`. `coverage` is the
#'   proportion of pixels in the selected scene group for each image.
#'
#' @details Scene groups are mapped to SCL codes as follows: clouds = 8, 9, 10;
#'   water = 6; snow = 11; shadows = 2, 3.
#'
#' @export
#' 
nc_scene_per_image <- function(nc, scene = "clouds"){
  sclt <- load_BandLayer(
    nc = nc, 
    band = "SCL", 
    monthFilter = NULL, 
    yearFilter = NULL
  )
  
  nT <- length(nc$t)
  
  if(scene == "clouds"){
    b <- c(8,9,10)
  } else if(scene == "water"){
    b <- 6
  } else if(scene == "snow"){
    b <- 11
  } else if(scene == "shadows"){
    b <- c(2,3)
  } else {
    stop("'scene' argument must be one of 'clouds', 'water', 'snow' or 'shadows'")
  }
  
  coverage <- c()
  for(i in seq_along(nc$t)){
    pixelScenes <- sclt$band[[i]]
    coverage <- c(coverage, sum(pixelScenes %in% b) / length(pixelScenes))
  }
  data.frame("date" = nc$t_date, "coverage" = coverage)
}
