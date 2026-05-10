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
  maxPixels = Inf,
  returnSinglePixels = TRUE, 
  days_around_ma = 30
)

# Correlation first
system.time(
  pClusters <- lakeRS::pixel_clusters(
    pixelDynamic = indexDynamic$pixelDynamics, 
    correlate_first = TRUE,
    k = 4
  )
)

k_i <- sort(unique(pClusters$clusterVector))
n <- summary(as.factor(pClusters$clusterVector))
DynamicStatList <- lapply(k_i, function(CLUSTER){
  lakeRS::dynamic_per_pixel(
    imageIndex = imageIndex, 
    nc = nc, 
    pixelFilter = 
      as.integer(
        gsub(pattern = "p_", replacement = "", names(pClusters$clusterVector))
      )[pClusters$clusterVector == CLUSTER],
    returnSinglePixels = FALSE, 
    days_around_ma = 30)$lakeDynamic
})

lakeRS::plot_dynamic(
  v_averageList = lapply(DynamicStatList, function(x){x$q_0.5}), 
  df_q50List = lapply(DynamicStatList, function(x){data.frame(x$q_0.25, x$q_0.75)}),
  lakeName = "Brates Lake", 
  TSnames = paste0("C", k_i)
)

clusterLayer <- get_clusterLayer(
  clusterVector = pClusters$clusterVector,
  nc = nc
)


lakeRS::map_layer(
  ncLayer = clusterLayer, 
  nc = nc, lowerLimits = k_i, 
  classColors = lakeRS::tenClusterColors$color
)

# No correlation first
system.time(
  pClusters <- lakeRS::pixel_clusters(
    pixelDynamic = indexDynamic$pixelDynamics, 
    correlate_first = FALSE,
    k = 4
  )
)

pClusters <- lakeRS::pixel_clusters(
  pixelDynamic = indexDynamic$pixelDynamics, 
  correlate_first = FALSE,
  k = 4
)

k_i <- sort(unique(pClusters$clusterVector))
n <- summary(as.factor(pClusters$clusterVector))
DynamicStatList <- lapply(k_i, function(CLUSTER){
  lakeRS::dynamic_per_pixel(
    imageIndex = imageIndex, 
    nc = nc, 
    pixelFilter = 
      as.integer(
        gsub(pattern = "p_", replacement = "", names(pClusters$clusterVector))
      )[pClusters$clusterVector == CLUSTER],
    returnSinglePixels = FALSE, 
    days_around_ma = 30)$lakeDynamic
})

lakeRS::plot_dynamic(
  v_averageList = lapply(DynamicStatList, function(x){x$q_0.5}), 
  df_q50List = lapply(DynamicStatList, function(x){data.frame(x$q_0.25, x$q_0.75)}),
  lakeName = "Brates Lake", 
  TSnames = paste0("C", k_i)
)

clusterLayer <- get_clusterLayer(
  clusterVector = pClusters$clusterVector,
  nc = nc
)

lakeRS::map_layer(
  ncLayer = clusterLayer, 
  nc = nc, lowerLimits = k_i, 
  classColors = lakeRS::tenClusterColors$color
)

