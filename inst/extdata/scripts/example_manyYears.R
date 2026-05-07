filePath <- "C:/Users/mzamzo/Documents/tmp/pcl/01_data/input/ndtri_openeo/Koerbaer Teich_Example_20180101-20241231"

# open netcdf connection
system.time(
  nc <- lakeRS::open_netcdf(filePath = filePath)
)

# Dynmaic
DynamicList <- list()
years <- 2018:2024
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
  
}

# plot alle years
x <- lapply(DynamicList, function(x){x$lakeDynamic$q_0.5})

lakeRS::plot_dynamic(
  v_averageList = x, 
  TSnames = years
)
# without reference year
d <- lakeRS::combine_years_dynamic(
  indexDynamicList = DynamicList, 
  years = 2018:2024
)
lakeRS::plot_dynamic(
  v_averageList = list(d$mean_of_median), 
  v_sdList = list(d$sd_over_median), 
  TSnames = "Years 2018 - 2024",
  lakeName = "Körbaer Teich"
)


# with references year
# Exclude reference year from averaging
d <- lakeRS::combine_years_dynamic(
  indexDynamicList = DynamicList, 
  years = 2018:2023
)
lakeRS::plot_dynamic(
  v_averageList = list(d$mean_of_median), 
  v_sdList = list(d$sd_over_median), 
  TSnames = "Years 2018 - 2023",
  df_reference = data.frame(
    "doy" = 1:365, 
    "value" = DynamicList$y_2024$lakeDynamic$q_0.5), 
  RefName = "2024", 
  lakeName = "Körbaer Teich"
)

# Index per image 
range(nc$t_date)

lakeIndexList <- list()
years <- 2018:2024
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


lakeRS::map_layer(
  ncLayer = matrix(
    data = classData$assessment$year_2020_class, 
    nrow = length(nc$y), ncol = length(nc$x)),
  nc = nc, 
  lowerLimits = lakeRS::tenClassColors$class,
  classColors = lakeRS::tenClassColors$color,
  plotLegend = TRUE) 

lakeRS::plot_layer(
  ncLayer = matrix(
    data = classData$assessment$year_2020_class, 
    nrow = length(nc$y), ncol = length(nc$x)), 
  nc = nc, 
  lowerLimits = lakeRS::tenClassColors$class, 
  classColors = lakeRS::tenClassColors$color)


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
  x = numericData$assessment$x[fromBadToGood[1:10]], 
  y = numericData$assessment$y[fromBadToGood[1:10]], 
  crs = nc$crs, labels = 1:10)

numericData[rowNumber,]
hist(numericData$assessment$trend_short)
median(numericData$trend_short, na.rm = TRUE)



