# 1. login to openEO -----------------------------------------------------------
library(openeo)

eodc <- connect(host = "https://openeo.dataspace.copernicus.eu/openeo/1.2")
available_collections <- list_collections()
describe_collection(available_collections$SENTINEL2_L2A)

available_processes <- list_processes()
"filter_spatial" %in% names(available_processes) # filter spatial is one of the processes

login() # login to https://dataspace.copernicus.eu/

# 2. Prepare a new job and download data ---------------------------------------
lakeName <- "Flughafensee"
lakeID <- "581967215"

tBeg <- "2023-01-01"
tEnd <- "2025-01-01"
p <- processes()

job_title <- paste0(
  lakeName, "_", 
  lakeID,  "_", 
  gsub(x = tBeg, pattern = "-", replacement = ""), "-", 
  gsub(x = tEnd, pattern = "-", replacement = "")
)

# bbox of Flughafensee 
# (bboxes for all lakes can be found in MinIO "pilot.1/lake_id_geometries")
top <- 52.5707298911985
bottom <- 52.5648291220453
left <- 13.2812849647087
right <- 13.2942989188758

# enlarge the rectangle by (library geosprhere is needed)
meters <- 20
top <- geosphere::destPoint(p = c(right, top),b = 0, d = meters)[2] # to the top
bottom <- geosphere::destPoint(p = c(right, bottom),b = 180, d = meters)[2] # to the bottom
left <- geosphere::destPoint(p = c(left, top),b = -90, d = meters)[1] # to the left
right <- geosphere::destPoint(p = c(right, top),b = 90, d = meters)[1] # to the right

# 3. Run job on back-end and download data --------------------------------------

lake_bbox <- p$load_collection(
  "id" = "SENTINEL2_L2A",
  "spatial_extent" = list(
    "crs" = 4326, 
    "east" =  right, 
    "north" =  top, 
    "south" = bottom,
    "west" = left),
  "temporal_extent" = list(tBeg, tEnd),
  "bands" = list("B05", "B02", "SCL"), # NULL for all bands
  "properties" = NULL
)

formats <- list_file_formats()

# netCDF as format
result_netcdf <- p$save_result(
  data = lake_bbox, 
  format = formats$output$netCDF 
)

job <- create_job(
  graph = result_netcdf,
  title = job_title
)

start_job(job = job)

# list jobs on backend, if the startet job is finished, it can be downloaded
jobs <- list_jobs()
jobs_df <- data.frame(jobs)
ready_for_download <- (jobs_df$title %in% job_title) & 
  (jobs_df$status == "finished")

# path to save data
netcdf_path <- "C:/Users/mzamzo/Documents/AD4GD"

if(any(ready_for_download)){
  download_results(
    job = jobs[[which(ready_for_download)]], 
    folder = file.path(netcdf_path, job_title) 
  )
  print(paste("Job", job_title, "downloaded and saved in", netcdf_path))
}

# 4. Load and prepare netCDF ---------------------------------------------------

nc_data <- ncdf4::nc_open(filename = file.path(netcdf_path, job_title, "openEO.nc"))  
# all variabiales included in the netcdf
names(nc_data$var)

# x, y and t
x <- ncdf4::ncvar_get(nc = nc_data, varid = "x")
y <- ncdf4::ncvar_get(nc = nc_data, varid = "y")
t <- ncdf4::ncvar_get(nc = nc_data, varid = "t")
t_date <- as.Date(t, origin = "1990-01-01")

b2 <- ncdf4::ncvar_get(nc = nc_data, varid = "B02")
b5 <- ncdf4::ncvar_get(nc = nc_data, varid = "B05")
scl <- ncdf4::ncvar_get(nc = nc_data, varid = "SCL")

# fill value does not need to be adjusted (is already NaN)
fillvalue <- ncdf4::ncatt_get(nc_data, "B02", "_FillValue")

ncdf4::nc_close(nc = nc_data)

month_of_images <- as.numeric(format(as.Date(t_date), "%m"))
year_of_images <- as.numeric(format(as.Date(t_date), "%Y"))


# 5. Calculate NDTrI per timestep (per image) -----------------------------------
# empty lists for all variables
b2_image <- b5_image <- scl_image <- ndtri_image <- list()
for(t_step in seq_along(t)){
  # negative reflactance need to be set to 0 before index is 
  # calculated, the same is done in the sentinel hub to harmonize
  # the data: https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/
  
  b2t <- b2_image[[t_step]] <- b2[, , t_step] 
  b2t[b2t < 0] <- 0
  b5t <- b5_image[[t_step]] <- b5[, , t_step]
  b5t[b5t < 0] <- 0
  sclt <- scl[, , t_step]
  # sometimes SCL pixel are NA, in order to calculate the proportion of scenes
  # those values are set to 0, which does not count for any other SCL 
  # (Scenes are coded between 1 and 11)
  sclt[is.na(sclt)] <- 0
  scl_image[[t_step]] <- sclt
  ndtri_image[[t_step]] <- (b5t - b2t) / (b5t + b2t) 
  # remove all cloud and snow pixels 
  ndtri_image[[t_step]][sclt %in% 7:11] <- NA
}

# 6. Filter for images based on the time ---------------------------------------
year <- 2023 # or 2024

used_for_ndtri <- 
  (month_of_images >= 4) & 
  (month_of_images <= 10) & 
  (year_of_images == year)

ndtri_seasonal_image <- ndtri_image[used_for_ndtri]
scl_seasonal_image <- scl_image[used_for_ndtri]

# 7. Filter for pixels per image based on the scene --------------------------------------
pixel_quality_threshold <- 0.8

# Proportion of all scenes per pixel (6 = water)
scenes <- lapply(1:11, function(s) {
  Reduce("+", lapply(scl_seasonal_image, function(image_scene){
    image_scene == s
  }))/ length(scl_seasonal_image)
})

# Sum of proportions for clouds and snow or ice
clouds_and_snow <- 
  scenes[[7]] + scenes[[8]] + scenes[[9]] + scenes[[10]] + scenes[[11]] + scenes[[3]]

# The proportion of water pixels after removing clouds and snow or ice
water_proportion <- scenes[[6]] / (1 - clouds_and_snow)

# Only water Scenes will be used for seasonal NDTrI 
ndtri_seasonal_image <- lapply(seq_along(ndtri_seasonal_image), function(i){
  # set all variables set to NA at all locations where scene is not water
  ndtri_seasonal_image[[i]][scl_seasonal_image[[i]] != 6] <- NA
  ndtri_seasonal_image[[i]]
})

# the amount of values which are not NA (--> Water pixels)
no_na_values <- 
  length(ndtri_seasonal_image) - Reduce("+",lapply(ndtri_seasonal_image, is.na))

# After counting the amount of values, set all NA values to 0 
# (Otherwise the pixel-wise summation --> and averaging, would not work)
ndtri_seasonal_image <- lapply(ndtri_seasonal_image, function(x){
  x[is.na(x)] <- 0
  x
})

# 8. Calculate seasonal NDTrI --------------------------------------------------
# Pixel-wise: Divide sum of image NDTrI by the amount of non NA values
ndtri <- Reduce("+", ndtri_seasonal_image) / no_na_values

# remove all uncertain pixels
use_pixel <- water_proportion >= pixel_quality_threshold
ndtri[!use_pixel] <- NA


# 9. Plot results (please load the functions below first) ----------------------
# Package raster is needed
library(raster)
plot_water_pixels(water_proportion = water_proportion)

plot_ndtri_season(
  ndtri_season = ndtri, x = x, y = y, 
  pal = df_col$color, cuts = df_col$ndtri, year = year)

# Here are two functions to plot the results and the definition of a color 
# palatte
{
  plot_water_pixels <- function(
    water_proportion, 
    above_values = c(0,0.3, 0.5, 0.7, 0.8, 0.9),
    above_colors = c("white","gray", "lightgreen","forestgreen",
                     "steelblue", "darkblue")
  ){
    r <- raster::raster(
      t(water_proportion), 
      xmn=min(x), 
      xmx=max(x), 
      ymn=min(y), 
      ymx=max(y))
    cuts <- c(above_values, 1.00000001) #set breaks
    
    ## every blue pixel will be used for analysis
    plot(r, xlim = c(min(x), max(x)), ylim = c(min(y), max(y)), 
         breaks = cuts, col = above_colors)
    
  }
  plot_ndtri_season <- function(
    ndtri_season, x, y, pal, cuts, year = NULL){
    r <- raster::raster(
      t(ndtri_season), 
      xmn=min(x), 
      xmx=max(x), 
      ymn=min(y), 
      ymx=max(y))
    plot(
      r, 
      xlim = c(min(x), max(x)), 
      ylim = c(min(y), max(y)), 
      breaks = cuts, 
      col = pal, main = paste0("NDTrI - ", year))
  }
  {
    oligo <- rgb(0, 51, 153, maxColorValue = 255)
    meso1 <- rgb(0, 176, 240, maxColorValue = 255)
    meso2 <- rgb(102, 153, 0, maxColorValue = 255)
    eu1 <- rgb(240, 220, 0, maxColorValue = 255)
    eu2 <- rgb(237, 179, 36, maxColorValue = 255)
    poly1 <- rgb(204, 102, 0, maxColorValue = 255)
    poly2 <- rgb(200, 0, 0, maxColorValue = 255)
    hyper <- rgb(128, 0, 128, maxColorValue = 255)
    no_water <- rgb(180, 180, 180, maxColorValue = 255)
    
    
    value_color <- data.frame(
      "estimated_average" = round(c(-1, -0.6, -0.385, -0.268, -0.153, -0.035, 0.082, 0.194, 0.37, 0.5, 1), 3),
      "color" = c(oligo, oligo, meso1, meso2, eu1, eu2, poly1, poly2, hyper, no_water, no_water))
    
    df_col <- merge(
      x = data.frame("ndtri" = round(seq(-1, 1, by = 0.001), 3)), 
      y = value_color, 
      by.x = "ndtri", 
      by.y = "estimated_average", 
      all.x = TRUE
    )
    
    
    
    col_defined <- which(!is.na(df_col$color))
    for(i in 2:length(col_defined)){
      col_from <- col2rgb(df_col$color[col_defined[i-1]])
      col_to <- col2rgb(df_col$color[col_defined[i]])
      n_inter <- col_defined[i] - col_defined[i-1] + 1
      
      col_step <- (col_to - col_from) /n_inter
      
      df_col$color[col_defined[i-1]:col_defined[i]] <- 
        sapply(1:n_inter, function(s) {
          new_color <- col_from + col_step * s
          rgb(new_color[1], new_color[2], new_color[3], maxColorValue = 255)
        })
    }
  }
  
}




