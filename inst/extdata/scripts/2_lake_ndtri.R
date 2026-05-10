
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

# Temporal aggregation
seasonIndex <- lakeRS::seasonal_index_per_pixel(
  imageIndex = imageIndex, 
  nc = nc, 
  water_scenes_only = TRUE, 
  pixelQualityThreshold = 0.8
)

# spatial aggregation
lakeIndex <- lakeRS::seasonal_index_per_lake(
  seasonIndex = seasonIndex, 
  lakeName = "Brates Lake",
  lakeID = "DS2"
) 

lakeRS::oneLake_df(lakeIndexList = list(lakeIndex))

lakeRS::plot_lake_index_histogram(
  lakeIndex = lakeIndex, 
  lakeName = "Brates Lake", 
  indexName = "NDTrI"
)

# yearly data set of lakewise data
yearlyLakes <- lakeRS::combine_years_lakeData(
  lakeIndexList = list(lakeIndex), 
  aggregationType = "modus"
)

# Plot
lakeRS::map_layer(
  ncLayer = lakeIndex$IndexPixel, 
  nc = nc, 
  lowerLimits = lakeRS::NDTrIColors$ndtri,
  classColors = lakeRS::NDTrIColors$color, 
  plotLegend = FALSE)

lakeRS::plot_layer(
  ncLayer = lakeIndex$IndexPixel, 
  nc = nc, 
  lowerLimits = lakeRS::NDTrIColors$ndtri,
  classColors = lakeRS::NDTrIColors$color, 
  plotLegend = FALSE)

# yearly data set of pixelwise data
yearlyPixels <- lakeRS::combine_years_pixelData(
  lakeIndexList = list(lakeIndex)
)
classData <- lakeRS::discreteClassAssessment(
  yearly_spread = yearlyPixels$indexTable,
  nClass = 10,
  proportionExtreme = 0.05
)

classLayer <- matrix(
  data = classData$assessment$year_2025_class, 
  nrow = length(nc$y), 
  ncol = length(nc$x)
)

lakeRS::map_layer(
  ncLayer = classLayer,
  nc = nc, 
  lowerLimits = lakeRS::tenClassColors$class,
  classColors = lakeRS::tenClassColors$color,
  plotLegend = TRUE
) 

lakeRS::plot_layer(
  ncLayer = classLayer,
  nc = nc, 
  lowerLimits = lakeRS::tenClassColors$class,
  classColors = lakeRS::tenClassColors$color,
  plotLegend = TRUE
) 





