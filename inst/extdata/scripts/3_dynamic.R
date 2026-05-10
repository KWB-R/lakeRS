filePath <- "C:/Users/mzamzo/Documents/tmp/Brates Lake_DS2_2025-01-01_2025-12-31_1"


# open netcdf connection
nc <- lakeRS::open_netcdf(filePath = filePath)

# Index per image 
imageIndex <- lakeRS::ndi_per_image(
  nc = nc, 
  year = 2025, 
  bandNames = c("B05", "B02"), 
  monthFilter = 1:12
)

indexDynamic <- lakeRS::dynamic_per_pixel(
  imageIndex = imageIndex, 
  nc = nc, 
  water_scenes_only = TRUE,
  maxPixels = Inf, 
  returnSinglePixels = TRUE, 
  pixelQualityThreshold = "byMedian",
  days_around_ma = 30
)

lakeRS::plot_dynamic(
  v_averageList = list(indexDynamic$lakeDynamic$q_0.5), 
  df_q50List = list(indexDynamic$lakeDynamic[,c("q_0.25", "q_0.75")]), 
  df_q95List = list(indexDynamic$lakeDynamic[,c("q_0.025", "q_0.975")]),
  lakeName = "Brates Lake", 
  ylab = "NDTrI per timestep"
)


# Chlorophyl-a (https://doi.org/10.1016/j.rse.2011.10.016)
imageIndex <- lakeRS::ndi_per_image(
  nc = nc, 
  year = 2025, 
  bandNames = c("B05", "B04"), 
  monthFilter = 1:12
)

indexDynamic <- lakeRS::dynamic_per_pixel(
  imageIndex = imageIndex, 
  nc = nc, 
  water_scenes_only = TRUE,
  maxPixels = Inf, 
  returnSinglePixels = TRUE, 
  pixelQualityThreshold = "byMedian",
  days_around_ma = 30
)

lakeRS::plot_dynamic(
  v_averageList = list(indexDynamic$lakeDynamic$q_0.5), 
  df_q50List = list(indexDynamic$lakeDynamic[,c("q_0.25", "q_0.75")]), 
  df_q95List = list(indexDynamic$lakeDynamic[,c("q_0.025", "q_0.975")]),
  lakeName = "Brates Lake", 
  ylab = "NDCI per timestep",
  TScolors = lakeRS::tenClusterColors$color[3]
)

# Turbidity (https://doi.org/10.1016/j.rse.2006.07.012)
imageIndex <- lakeRS::ndi_per_image(
  nc = nc, 
  year = 2025, 
  bandNames = c("B04", "B03"), 
  monthFilter = 1:12
)

indexDynamic <- lakeRS::dynamic_per_pixel(
  imageIndex = imageIndex, 
  nc = nc, 
  water_scenes_only = TRUE,
  maxPixels = Inf, 
  returnSinglePixels = TRUE, 
  pixelQualityThreshold = "byMedian",
  days_around_ma = 30
)

lakeRS::plot_dynamic(
  v_averageList = list(indexDynamic$lakeDynamic$q_0.5), 
  df_q50List = list(indexDynamic$lakeDynamic[,c("q_0.25", "q_0.75")]), 
  df_q95List = list(indexDynamic$lakeDynamic[,c("q_0.025", "q_0.975")]),
  lakeName = "Brates Lake", 
  ylab = "NDTI per timestep",
  TScolors = lakeRS::tenClusterColors$color[2]
)


# NDWI 
imageIndex <- lakeRS::ndi_per_image(
  nc = nc, 
  year = 2025, 
  bandNames = c("B03", "B08"), 
  monthFilter = 1:12
)

indexDynamic <- lakeRS::dynamic_per_pixel(
  imageIndex = imageIndex, 
  nc = nc, 
  water_scenes_only = TRUE,
  maxPixels = Inf, 
  returnSinglePixels = TRUE, 
  pixelQualityThreshold = "byMedian",
  days_around_ma = 30
)

lakeRS::plot_dynamic(
  v_averageList = list(indexDynamic$lakeDynamic$q_0.5), 
  df_q50List = list(indexDynamic$lakeDynamic[,c("q_0.25", "q_0.75")]), 
  df_q95List = list(indexDynamic$lakeDynamic[,c("q_0.025", "q_0.975")]),
  lakeName = "Brates Lake", 
  ylab = "NDWI per timestep",
  TScolors = lakeRS::tenClusterColors$color[2]
)



