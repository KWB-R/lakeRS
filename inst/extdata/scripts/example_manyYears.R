filePath <- "C:/Users/mzamzo/Documents/tmp/pcl/01_data/input/ndtri_openeo/Brates Lake_DS2_20180101-20251201"

# open netcdf connection
system.time(
  nc <- lakeRS::open_netcdf(filePath = filePath)
)

# Dynmaic
DynamicList <- list()
years <- 2018:2025
for(year in years){
  cat(paste0("Processing year ", year, " ...\n"))
  imageIndex <- lakeRS::ndi_per_image(
    nc = nc, 
    year = year, 
    bandNames = c("B05", "B02")
  )
  DynamicList[[paste0("y_", year)]] <- lakeRS::dynamic_per_pixel(
    imageIndex = imageIndex, 
    nc = nc, 
    maxPixels = Inf, 
    returnSinglePixels = FALSE, 
    days_around_ma = 30)
  
  # plot alle years
  x <- lapply(DynamicList, function(x){x$lakeDynamic$q_0.5})
  lakeRS::plot_dynamic(
    v_averageList = x, 
    TSnames = names(x)
  )
  
  d <- lakeRS::combine_years_dynamic(
    indexDynamicList = DynamicList, 
    years = 2018:2024
  )
  
  lakeRS::plot_dynamic(
    v_averageList = list(d$mean_of_median), 
    v_sdList = list(d$sd_over_median), 
    TSnames = "Years 2018 - 2024",
    df_reference = data.frame(
      "doy" = 1:365, 
      "value" = DynamicList$y_2025$lakeDynamic$q_0.5), 
    RefName = "2025", 
    lakeName = "Lake Brates"
  )
  
  
  
  
}

# Index per image 
range(nc$t_date)

lakeIndexList <- list()
years <- 2018:2025
for(year in years){
  cat(paste0("Processing year ", year, " ...\n"))
  imageIndex <- lakeRS::ndi_per_image(
    nc = nc, 
    year = year, 
    bandNames = c("B05", "B02")
  )
  # Temporal aggregation
  seasonIndex <- lakeRS::seasonal_index_per_pixel(
    imageIndex = imageIndex, 
    nc = nc, 
    water_scenes_only = TRUE, 
    pixelQualityThreshold = 0.8
  )
  lakeIndexList[[paste0("year_", year)]] <- lakeRS::seasonal_index_per_lake(
    seasonIndex = seasonIndex, 
    lakeName = "Brates",
    lakeID = "DS2"
  ) 
}
rm(imageIndex)

# yearly data set of lakewise data
yearlyLakes <- lakeRS::combine_years_lakeData(lakeIndexList = lakeIndexList)

# yearly data set of pixelwise data
yearlyPixels <- lakeRS::combine_years_pixelData(lakeIndexList = lakeIndexList)
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

numericData <- lakeRS::numericAssessment(
  yearly_spread = yearlyPixels$indexTable, 
  statusYears = 3, 
  shortTermYears = 3, 
  longTermYears = 7
)

fromBadToGood <- order(numericData$assessment$trend_short, decreasing = TRUE)
lakeRS::plot_numeric_assessment(
  numericData = numericData, 
  rowNumber = fromBadToGood[1]
)
lakeRS::show_pixels(
  x = numericData$assessment$x[fromBadToGood[1]], 
  y = numericData$assessment$y[fromBadToGood[1]], 
  crs = nc$crs)

numericData[rowNumber,]
hist(numericData$trend_short)
median(numericData$trend_short, na.rm = TRUE)

xScene <- lakeRS::nc_scene_per_image(
  nc = nc, 
  scene = "water"
)

cMax <- max(xScene$coverage) 
dev.new(noRStudioGD = TRUE, width = 13.4, height = 5.1)
lakeRS::plot_scene_coverage(
  vDate = xScene$date, 
  vCoverage = xScene$coverage, 
  upperLimits = round(
    c(0.05 * cMax, 0.2 * cMax, 0.5 * cMax,  0.8 * cMax, 0.9 * cMax, cMax), 3)
)

# Scene analysis per pixel
nc <- lakeRS::nc

ncf <- lakeRS::nc_time_filter(
  nc = nc, 
  tBeg = "2020-04-01", 
  tEnd = "2020-12-31"
)

scl_image <- lapply(
  seq_along(ncf$t_date), 
  lakeRS::load_bandLayer, 
  nc = ncf, 
  bandName = "SCL"
)
scl_image <- lapply(scl_image, function(x){
  x[is.na(x)] <- 0
  x
})
sceneProportion <- lakeRS::waterscene_proportion(
  scl_image = scl_image
)

# --> Map Layer muss gemacht werden
lakeRS::map_layer(
  ncLayer = sceneProportion$water,
  nc = ncf,
  lowerLimits = c(0,0.1, 0.3, 0.5, 0.7, 0.8, 0.9),
  classColors = c("white","gray80", "gray60", "chartreuse1","chartreuse4",
                  "dodgerblue", "dodgerblue4"),
  legendTitle = "Water Classification Quality"
)

years <- as.numeric(unique(format(x = nc$t_date, "%Y")))
matrixDim <- c(length(nc$y), length(nc$x))

