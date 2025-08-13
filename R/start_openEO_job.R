#' Start Job on openEO plattform
#' 
#' This function will implement and start a job on openEO. You need to be
#' connected and logged in to run this function.
#' 
#' @param title A character defining the job title (can be a lake name, id). It
#' is advised to use a unique name (i.e. including a timestamp) that is used on
#' openEO and as a folder on the local machine
#' @param bbox Numerical vector defining the bounding box in the coordinate 
#' reference system as defined in crs.The order of the values is north, east, 
#' south, west.
#' @param tBeg,tEnd Dates in the format "YYYY-MM-DD" which define the temporal
#' extent of the data to be downloaded. Left side (tBeg) is included, right side
#' (tEnd) is excluded from range.
#' @param crs A numerical value defining the coordinate reference system. By
#' default this is 4326 (WGS 84). See here for further information:
#' https://epsg.io/4326
#' @param collection One of the available collections on openEO 
#' (SENTINEL2_L2A is default). The collections can be listed by
#' [openeo::list_collections()]
#' @param bands A list of characters defining The bands to be downloaded. 
#' For the normalized difference trophic index bands 2, 5 and the scene 
#' classification (SCL) are required. Set to NULL to download all bands.
#' @param outputFormat On of the available output formats (netcdf is default). 
#' The formats can be listed by [openeo::list_file_formats()]. Other alternatives
#' for higher dimensional data cubes are "GeoJSON" or "GTiff".
#' @param uniqueTitle If TRUE, the previous jobs are checked. If the title has 
#' already been used, it is suffixed by an underscore and the lowest possible 
#' number
#' @return 
#' This function only returns the title of the job that is started on openEO.
#' To download the job use function [download_openEO_job]
#' 
#' @export
#' 
#' @importFrom openeo processes list_file_formats list_jobs create_job start_job
#' 
start_openEO_job <- function(
    title,
    bbox,
    tBeg, tEnd,
    crs = 4326,
    collection = "SENTINEL2_L2A", 
    bands = list("B02", "B05", "SCL"),
    outputFormat = "netCDF",
    uniqueTitle = TRUE
){
  p <- openeo::processes() 
  
  dataDefinition <- p$load_collection(
    "id" = collection,
    "spatial_extent" = list(
      "crs" = crs, 
      "north" = bbox[1], 
      "east" = bbox[2], 
      "south" = bbox[3],
      "west" = bbox[4]),
    "temporal_extent" = list(tBeg, tEnd),
    "bands" = bands, 
    "properties" = NULL
  )
  
  #f = openeo::list_file_formats()
  result_netcdf = p$save_result(
    data = dataDefinition, 
    format = outputFormat
  )
  if(uniqueTitle){
    previousJobs <- as.data.frame(openeo::list_jobs())
    pt <- previousJobs$title
    same_name <- pt[grep(pattern = title, x = pt)]
    if(length(same_name) > 0L){
      if(title %in% pt){
        i <- 1
        title <- paste0(title, "_", i)
        
        while(title %in% pt){
          title <- sub(
            pattern = paste0("_", i, "$"), 
            replacement = paste0("_", i + 1), 
            x = title
          )
          i <- i + i
        }
      }
    }
  }
  
  job = openeo::create_job(
    graph = result_netcdf,
    title = title)
  
  openeo::start_job(job = job)
  
  return(title)
}
