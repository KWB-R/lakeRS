#' Calculates the proportion of a specified scene in an image
#' 
#' @param nc The output of [open_netcdf()] function
#' @param scene Character defining the scene. Can be one of "clouds", "water",
#' "snow" or "shadows"
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
