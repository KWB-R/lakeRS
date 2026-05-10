# Process data on Copernicus web editor and download
# (account for copernicus dataspace required)
if(!("openeo" %in% installed.packages())){
  install.packages("openeo")
}
eodc <- connect(host = "https://openeo.dataspace.copernicus.eu/openeo/1.2")
openeo::login()

# lake meta data, temporal and spatial filter 
lakeName <- "Brates Lake"
lakeID <- "DS2"

tBeg <- "2025-01-01"
tEnd <- "2025-12-31"

# 1. Bounding Box from point 
geom <- lakeRS::bbox_from_point(
  lat = 45.48344, 
  lon = 28.06878, 
  meters = 500
)


# 2. Bounding Box from Rectangle
geom <- lakeRS::bbox_from_rectangle(
  top_lat = 45.505707, 
  bottom_lat = 45.461176, 
  left_lon = 28.037879, 
  right_lon = 28.099677, 
  meters = 100
)

# 3. Bounding Box Polygon (TBA)
geom <- list(
  c(45.505754, 28.037929), c(45.504479, 28.102674),c(45.462632, 28.101410), 
  c(45.460962, 28.086337),c(45.461727, 28.065711), c(45.467570, 28.045977)
)


lakeRS::display_geometry(
  geom = geom, 
  zoom = 13
)

jobTitle <- lakeRS::start_openEO_job(
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

