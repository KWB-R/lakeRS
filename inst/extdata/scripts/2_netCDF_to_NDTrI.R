inputPath <- "C:/Users/mzamzo/Documents/AD4GD/remote_sensing/input"
jobTitle <- "Brandiser Badesee__2018-01-01_2024-12-31"

# load and process netCDF data
nc <- lakeRS::load_netcdf(
  title = jobTitle, 
  path = inputPath
)
ncImage <- lakeRS::data_per_image(nc = nc)
sceneProportion <- lakeRS::waterscene_proportion(scl_image = ncImage[["SCL"]])

lakeRS::plot_layer(
  ncLayer = sceneProportion$water, 
  nc = nc, 
  aboveValues = c(0,0.3, 0.5, 0.7, 0.8, 0.9), 
  aboveColors = c("white","gray", "lightgreen","forestgreen",
                  "steelblue", "darkblue"), 
  legendTitle = "Water Scene Proportion"
)

y2018 <- lakeRS::NDTrI(
  nc = nc, 
  ncImage = ncImage, 
  waterLayer = sceneProportion$water, 
  year = 2018)

lakeRS::plot_layer(
  ncLayer = y2018$NDTrI, 
  nc = nc, 
  aboveValues = lakeRS::NDTrIColors$ndtri, 
  aboveColors = lakeRS::NDTrIColors$color, 
  plotLegend = FALSE
)

