#' Calculate water-scene proportions from SCL images
#'
#' Computes per-pixel proportions for all Sentinel-2 SCL classes and derives a
#' water-scene proportion after accounting for disturbance classes.
#'
#' @param scl_image A list of SCL matrices, typically loaded from the SCL band of
#'   an [open_netcdf()] object for the same images used in an index calculation.
#' 
#' @return A list with `water`, `NoFalseDisturbance`, and `allScenes`. `water` is
#'   the derived water-scene proportion per pixel; `NoFalseDisturbance` is the
#'   proportion of pixels assigned to allowed disturbance classes; `allScenes` is
#'   a list of per-class proportions.
#'   
#' @details The water proportion is defined as all pixels that are "correctly" classified
#'   as water. Thus, disturbances that occur but are not a false classification 
#'   like clouds, snow or topographic impacts need to be removed before. After
#'   removing those pixels, the water classifiation should be 100% for a perfect 
#'   pixel
#' 
#' @export
#' 
waterscene_proportion <- function(scl_image){
  
  scenes <- lapply(lakeRS::sceneIDs$scl, function(s) {
    Reduce("+", lapply(scl_image, function(image_scene){
      image_scene == s
    })) / length(scl_image)
  })
  
  AllowedDisturbingScenes <- c(0, 2, 3, 7, 8, 9, 10, 11)
  
  cloudsSnowTopography <- scenes[[AllowedDisturbingScenes[1] + 1]]
  for(s in AllowedDisturbingScenes[-1]){
    cloudsSnowTopography <- cloudsSnowTopography + scenes[[s+1]]
  }
 
  # The proportion of water pixels after removing clouds and snow or ice
  water <- scenes[[6+1]] / (1 - cloudsSnowTopography)
  
  list("water" = water,
       "NoFalseDisturbance" = cloudsSnowTopography, 
       "allScenes" = scenes)
}

