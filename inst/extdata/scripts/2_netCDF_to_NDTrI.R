RSPath <- "C:/Users/mzamzo/Documents/themen/RS"
available_downloads <- dir(file.path(RSPath, "input"))
jobTitle <- available_downloads[2]
lakeInfo <- strsplit(jobTitle, "_")[[1]][1:2]



# load and process netCDF data
nc <- lakeRS::load_netcdf(
  title = jobTitle, 
  path = file.path(RSPath, "input")
)
ncImage <- lakeRS::data_per_image(nc = nc)

# sceneProportion <- lakeRS::waterscene_proportion(scl_image = ncImage[["SCL"]])
# 
# lakeRS::plot_layer(
#   ncLayer = sceneProportion$water, 
#   nc = nc, 
#   aboveValues = c(0,0.3, 0.5, 0.7, 0.8, 0.9), 
#   aboveColors = c("white","gray", "lightgreen","forestgreen",
#                   "steelblue", "darkblue"), 
#   legendTitle = "Water Scene Proportion"
# )

ndtriPixels <- lakeRS::get_pixel_NDTrI(
  nc = nc, 
  ncImage = ncImage, 
  years = 2018:2024, 
  lakeInfo = lakeInfo, 
  seasonMonths = 4:10, 
  water_scenes_only = TRUE, 
  pixelQualityThreshold = 0.8
)

ndtriLake <- lakeRS::aggregate_NDTrI(ndtriPixels = ndtriPixels)

# lakeRS::plot_layer(
#   ncLayer = ndtriPixels$y2019$NDTrI, 
#   nc = nc, 
#   aboveValues = lakeRS::NDTrIColors$ndtri, 
#   aboveColors = lakeRS::NDTrIColors$color, 
#   plotLegend = TRUE, 
#   legendTitle = "NDTrI per Pixel", 
#   zoom = 17
# )

lakeRS::save_lake_data(
  RSPath = RSPath, 
  nc = nc, 
  ncImage = ncImage, 
  sceneProportion = lakeRS::waterscene_proportion(scl_image = ncImage[["SCL"]]), 
  ndtriPixels = ndtriPixels, 
  ndtriLake = ndtriLake)
