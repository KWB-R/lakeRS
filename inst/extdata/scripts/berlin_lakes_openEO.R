# install.packages("openeo")
library(openeo)
library(geosphere)

# functions 
enlarge_rectangle <- function(top, bottom, left, right, meters){
  right2 <- geosphere::destPoint(p = c(right, top),b = 90, d = meters)[1] # to the right
  top2 <- geosphere::destPoint(p = c(right, top),b = 0, d = meters)[2] # to the top
  left2 <- geosphere::destPoint(p = c(left, top),b = -90, d = meters)[1] # to the left
  bottom2 <- geosphere::destPoint(p = c(right, bottom),b = 180, d = meters)[2] # to the bottom
  c("top" = top2, "bottom" = bottom2, "left" = left2, "right" = right2) 
}

# load data
geo_data <- "Y:/iGB/Projects/AD4GD/Exchange/01_data/03_output/ID_tables"
lake_rectangles <- read.csv(
  file = file.path(geo_data, "lake_id_bbox.csv"), 
  sep = ";", 
  dec = "."
)

eodc <- connect(host = "https://openeo.dataspace.copernicus.eu/openeo/1.2")
available_collections <- list_collections()
describe_collection(available_collections$SENTINEL2_L2A)
describe_collection(available_collections$SENTINEL2_L1C) # no scl in L1C products

available_processes <- list_processes()
"filter_spatial" %in% names(available_processes) # filter spatial is in there

# magic login function (opens the browser to log in with copernicus dataspace credentials)
login()

tBeg <- "2018-01-01"
tEnd <- "2025-01-01"
p <- processes()


downloaded_jobs <- dir(file.path(
  "Y:/iGB/Projects/AD4GD/Exchange",
  "01_data/01_input/satellite_data/openeo/netcdf"))


job_titles <- c()
# a maximum of 2 Jobs at a time is allowed

for(i in 1:397){
  job_i <- paste0(lake_rectangles$Lake_name[i], "_", lake_rectangles$ID[i], 
         "_", gsub(x = tBeg, pattern = "-", replacement = ""), 
         "-", gsub(x = tEnd, pattern = "-", replacement = ""))
  
  if(!(job_i %in% downloaded_jobs)){
    print(paste0(i, " processing"))
    job_titles <- c(job_titles,  job_i)
    
    
    rect <- enlarge_rectangle(
      top = lake_rectangles$top[i], 
      bottom = lake_rectangles$bottom[i],
      left = lake_rectangles$left[i], 
      right = lake_rectangles$right[i],
      meters = 20
    )
    
    lake_bbox <- p$load_collection(
      "id" = "SENTINEL2_L2A",
      "spatial_extent" = list(
        "crs" = 4326, 
        "east" =  rect["right"], 
        "north" =  rect["top"], 
        "south" = rect["bottom"],
        "west" = rect["left"]),
      "temporal_extent" = list(tBeg, tEnd),
      "bands" = list("B05", "B02", "SCL"), # NULL for all bands
      "properties" = NULL
    )
    formats = list_file_formats()
    # netCDF as format
    result_netcdf = p$save_result(
      data = lake_bbox, 
      format = formats$output$netCDF 
    )
    jobs <- list_jobs()
    currently_running <- sum(sapply(jobs, function(x){x$status == "running"}))
    
    while(currently_running > 1){
      Sys.sleep(time = 120)
      jobs <- list_jobs()
      currently_running <- sum(sapply(jobs, function(x){x$status == "running"}))
    }
    
    # Execute it on the back-end ("batch-job"), only if it was not already done
    new_title <- job_titles[length(job_titles)]
    if(!(new_title %in% data.frame(jobs)$title)){
      job = create_job(
        graph = result_netcdf,
        title = job_titles[length(job_titles)])
      start_job(job = job)
      print(paste(new_title, "startet on backend."))
      Sys.sleep(120)
    }
    
    jobs <- list_jobs()
    jobs_df <- data.frame(jobs)
    
    ready_for_download <- jobs_df$title %in% job_titles & jobs_df$status == "finished"
    if(any(ready_for_download)){
      for(job_index in which(ready_for_download)){
        jt <- jobs_df$title[job_index]
        download_results(
          job = jobs[[job_index]], 
          folder = file.path(
            "Y:/iGB/Projects/AD4GD/Exchange",
            "01_data/01_input/satellite_data/openeo/netcdf", jt) 
        )
        job_titles <- job_titles[job_titles != jt]
        print(paste(jt, "downloaded and removed from pending job titles."))
      }
    }
  }
  
 
}



  
  
  
  
  