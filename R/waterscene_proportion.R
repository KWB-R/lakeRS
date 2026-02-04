#' Proprtions of all scenes and dervied water scene proportion
#'
#' @param scl_image The SCL layer of the image-wise data, created by one of the
#' index per image functions: [ndi_per_image()], [additive_per_image()]
#' 
#' @details
#' The water proportion is defined as all pixels that are "correctly" classified
#' as water. Thus, disturbances that occur but are not a false classification 
#' like clouds, snow or topographic impacts need to be removed before. After
#' removing those pixels, the water classifiation should be 100% for a perfect 
#' pixel
#' 
#' @return
#' A list of 3:
#'  - the water scene proportion per pixel
#'  - the disturbance scene proportion per pixel
#'  - A list of all individual scene proportions per pixel
#' 
#' @export
#' 
waterscene_proportion <- function(scl_image){
  scenes <- lapply(0:11, function(s) {
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

