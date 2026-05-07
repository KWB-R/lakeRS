# Example Koerbaer Teich in Germany, Brandenburg
# Temporal and spatial data, lake meta data
lakeName <- "Koerbaer Teich"
lakeID <- "lakeRS-example"

# 1. Bounding Box from point 
geom <- lakeRS::bbox_from_point(
  lat = 51.815136, 
  lon = 13.402570 , 
  meters = 500)
lakeRS::display_geometry(geom = geom)

# 2. Bounding Box from Rectangle
geom <- lakeRS::bbox_from_rectangle(
  top_lat = 51.817410, 
  bottom_lat = 51.812633, 
  left_lon = 13.395993, 
  right_lon = 13.408728, 
  meters = 0)
lakeRS::display_geometry(geom = geom)

# 3. Bounding Box Polygon (TBA)
# Especially for lakes that do not have a shore parallel to lon/lat axis this
# can safe datasize efficiently. Although openEO processes a rectangle 
# parallel to the axis it masks the output file by that polygon and further
# compression lead to much lower data size.

tBeg <- "2020-01-01"
tEnd <- "2021-01-01"

# Process data on Copernicus web editor and download
# (account for copernicus dataspace required)
library(openeo)
eodc <- connect(host = "https://openeo.dataspace.copernicus.eu/openeo/1.2")
login()

jobTitle <- start_openEO_job(
  title = paste(lakeName, lakeID, tBeg, tEnd, sep = "_"), 
  geom = geom, 
  bands = list(
    "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", 
    "B11", "B12", "SCL"), 
  tBeg = tBeg, 
  tEnd = tEnd
)

# More information on the job, its progess and debugging options can be found
# here: https://openeo.dataspace.copernicus.eu/
# Two jobs can run in parallel on openEO
lakeRS::download_openEO_job(
  title = jobTitle, 
  path = file.path(getwd(), "input")
)

# load netCDF file (no example without a netCDF file in the package --> Maybe very small file in ins/extdata)
# Scene analysis per time

filePath <- "C:/Users/mzamzo/Documents/tmp/pcl/01_data/input/ndtri_openeo/Koerbaer Teich_lakeRS-example_2020-01-01_2021-01-01"
filePath <- "C:/Users/mzamzo/Documents/tmp/pcl/01_data/input/ndtri_openeo/Brates Lake_DS2_20180101-20251201"

# further input
lakeName <- 
  # "Lake Brates" 
  "Körbaer Teich"
lakeID <- 
  # "DS2" 
  "800015388119" 
indexName <- 
  "NDTrI"
  #"NDCI"
  # "NDTI"
  # "NDSSI"

indexBands <- 
  c("B05", "B02")
  # c("B05", "B03")
  # c("B04", "B03")
  # c("B02", "B08")

#
# open netcdf connection
nc <- lakeRS::open_netcdf(filePath = filePath)

# General information on image and scene classification frequency
xScene <- lakeRS::nc_scene_per_image(nc = nc, scene = "clouds")
lakeRS::plot_scene_coverage(
  vDate = xScene$date, 
  vCoverage = xScene$coverage
)

lakeRS::sceneIDs

sceneProportion <- lakeRS::waterscene_proportion(scl_image = lakeRS:::load_BandLayer(nc = nc, band = "SCL")$band)
lakeRS::plot_layer(
  ncLayer = sceneProportion$water,
  nc = nc,
  lowerLimits = c(0,0.1, 0.3, 0.5, 0.7, 0.8, 0.9),
  classColors = c("white","gray80", "gray60", "chartreuse1","chartreuse4",
                  "dodgerblue", "dodgerblue4"),
  legendTitle = "Water Classification Quality"
)

lakeRS::plot_layer(
  ncLayer = sceneProportion$allScenes[[5]],
  nc = nc,
  lowerLimits = c(0,0.02,0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,1) ,
  classColors = c("white","gray80","gray60", "aquamarine1", "aquamarine3",
                  "chartreuse1", "chartreuse3", "darkgreen", "goldenrod1",
                  "chocolate1", "orangered1"),
  legendTitle = paste0("Proportion of '", lakeRS::sceneIDs$name[5],"'")
)

# Index per image 
range(nc$t_date)
imageIndex <- lakeRS::ndi_per_image(
  nc = nc, 
  year = 2020, 
  bandNames = indexBands
)

# yearly dynamic
{
  indexDynamic <- lakeRS::dynamic_per_pixel(
    imageIndex = imageIndex, 
    nc = nc, 
    maxPixels = Inf, 
    returnSinglePixels = TRUE, 
    days_around_ma = 30)
  
  lakeRS::plot_dynamic(
    v_averageList = list(indexDynamic$lakeDynamic$q_0.5), 
    df_q50List = list(indexDynamic$lakeDynamic[,c("q_0.25", "q_0.75")]), 
    df_q95List = list(indexDynamic$lakeDynamic[,c("q_0.025", "q_0.975")]),
    lakeName = lakeName, 
    ylab = paste0(indexName, " per timestep"))
  
  lakeRS::plot_dynamic(
    v_averageList = list(indexDynamic$lakeDynamic$q_0.5), 
    df_q50List = list(indexDynamic$lakeDynamic[,c("q_0.25", "q_0.75")]), 
    df_q95List = NULL,
    lakeName = lakeName, 
    ylab = paste0(indexName, " per timestep"))
  
  
  lakeRS::best_nk(pixelDynamic = indexDynamic$pixelDynamics)
  k <- 4
  
  pClusters <- lakeRS::pixel_clusters(
    pixelDynamic = indexDynamic$pixelDynamics, 
    nc = nc, correlate_first = TRUE,
    k = k)
  
  DynamicStatList <- lapply(1:k, function(CLUSTER){
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
    lakeName = lakeName, 
    TSnames = paste0("C", 1:k)
  )
    
  pClusters$clusterVector
 
}

# Temporal aggregation
seasonIndex <- lakeRS::seasonal_index_per_pixel(
  imageIndex = imageIndex, 
  nc = nc, 
  water_scenes_only = TRUE, 
  pixelQualityThreshold = 0.8
)

# Plots ------------------------------------------------------------------------
if(FALSE){
  lakeRS::map_layer(
    ncLayer = seasonIndex$RSindex, 
    nc = nc, 
    lowerLimits = lakeRS::NDTrIColors$ndtri,
    classColors = lakeRS::NDTrIColors$color, 
    plotLegend = FALSE)
}

# spatial aggregation
lakeIndex <- lakeRS::seasonal_index_per_lake(
  seasonIndex = seasonIndex, 
  lakeName = lakeName,
  lakeID = "800015388119"
) 

# yearly data set of lakewise data
yearlyLakes <- lakeRS::combine_years_lakeData(
  lakeIndexList = list(lakeIndex), 
  aggregationType = "modus"
)

# Plots ------------------------------------------------------------------------
if(FALSE){
  
  lakeRS::plot_lake_index_histogram(
    lakeIndex = lakeIndex, 
    lakeName = lakeName, 
    indexName = indexName
  )
  
  # some plot as above
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
}



# yearly data set of pixelwise data
yearlyPixels <- lakeRS::combine_years_pixelData(lakeIndexList = list(lakeIndex))
classData <- lakeRS::discreteClassAssessment(
  yearly_spread = yearlyPixels$indexTable
)

{
  # some plot as above
  lakeRS::map_layer(
    ncLayer = matrix(
      data = classData$assessment$year_2020_class, 
      nrow = length(nc$y), ncol = length(nc$x)),
    nc = nc, 
    lowerLimits = lakeRS::tenClassColors$class,
    classColors = lakeRS::tenClassColors$color,
    plotLegend = FALSE) 
}

numData <- lakeRS::numericAssessment(
  yearly_spread = yearlyPixels$indexTable
)






