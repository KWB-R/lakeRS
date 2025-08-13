# download and process netCDF data based on a point coordinate
eodc <- connect(host = "https://openeo.dataspace.copernicus.eu/openeo/1.2")
login()

lakeName <- "Brandiser Badesee"
lakeID <- NULL
tBeg <- "2018-01-01"
tEnd <- "2024-12-31"
lat <- 13.168516
lon <- 51.790693
outputPath <- "C:/Users/mzamzo/Documents/AD4GD/remote_sensing/input"

# prepare bounding box
bbox <- lakeRS::bbox_from_point(
  lon = lon,
  lat = lat
)
lakeRS::display_bbox(bbox = bbox)

# run and download openEO Job
jobTitle <- lakeRS::start_openEO_job(
  title = paste(lakeName, lakeID, tBeg, tEnd, sep = "_"), 
  bbox = bbox, 
  tBeg = tBeg, tEnd = tEnd
)
lakeRS::download_openEO_job(
  title = jobTitle, 
  path = outputPath
)

# load and process netCDF data
nc <- lakeRS::load_netcdf(
  title = jobTitle, 
  path = outputPath
)
ncImage <- lakeRS::data_per_image(nc = nc)
waterProportion <- lakeRS::waterscene_proportion(scl_image = ncImage[["SCL"]])

