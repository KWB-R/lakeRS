RSPath <- "C:/Users/mzamzo/Documents/ProCleanLakes/data/satellite"
available_downloads <- dir(file.path(RSPath))
jobTitle <- available_downloads[3]
lakeInfo <- strsplit(jobTitle, "_")[[1]][1:2]



# load and process netCDF data
nc <- lakeRS::load_netcdf(
  filePath = file.path(RSPath, jobTitle),
  vars = "all"
)

# filter for a time period in which the index is calculated 
# For ndtri this can be a whole season or a year, but it can also be a
# single image

ncf <- lakeRS::nc_time_filter(nc = nc, tBeg = "2024-08-05", tEnd = "2024-08-05")

cloudScene <- lakeRS::nc_scene_per_image(nc = ncf, scene = "clouds")
lakeRS::plot_image_dates(vDate = ncf$t_date, vCoverage = cloudScene$coverage)

ncImage <- lakeRS::ndi_per_image(nc = ncf)



ncImage2 <- lakeRS::additive_per_image(nc = ncf)
lakeRS::plot_layer(
  ncLayer = ncImage2$RSindex[[1]],
  nc = ncf,
  zoom = 11
)


sceneProportion <- lakeRS::waterscene_proportion(scl_image = ncImage[["SCL"]])

lakeRS::plot_layer(
  ncLayer = sceneProportion$water,
  nc = nc,
  aboveValues = c(0,0.3, 0.5, 0.7, 0.8, 0.9),
  aboveColors = c("white","gray", "lightgreen","forestgreen",
                  "steelblue", "darkblue"),
  legendTitle = "Water Scene Proportion"
)

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



lakeRS::save_lake_data(
  outputPath = RSPath, 
  nc = nc, 
  ncImage = ncImage, 
  sceneProportion = lakeRS::waterscene_proportion(scl_image = ncImage[["SCL"]]), 
  ndtriPixels = ndtriPixels, 
  ndtriLake = ndtriLake)
