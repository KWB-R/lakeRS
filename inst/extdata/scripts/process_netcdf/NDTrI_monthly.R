library(ncdf4)
library(raster)
library(sf)

pp <- "Y:/iGB/Projects/AD4GD"
dp <- "Exchange/01_data"
raw_dp <- "01_input/satellite_data/openeo/netcdf"
result_dp <- "03_output/monthly_ndtri"

input_path <- 
  # file.path(pp, dp, raw_dp)
  "Y:/iGB/Projects/ProCleanLakes/Exchange/01_data/01_input/ndtri_openeo"


output_path <-file.path(pp, dp, result_dp)

geoms <- read.csv(
  file = file.path(pp, dp, "03_output/ID_tables", "lake_id_geom.csv"),
  sep = ";"
)

folders <- dir(input_path)
folders

l <- 4

if(FALSE){
  lake <- folders[l]
  
  save_files_here <- 
    file.path(output_path, 
              paste(unlist(strsplit(lake, "_"))[1:2], collapse = "_"))
  
  if(!dir.exists(save_files_here)){
    dir.create(path = save_files_here, showWarnings = FALSE)
  }
  
  lake_data <- strsplit(lake, split = "_")[[1]]
  lake_id <- lake_data[2]
  
  nc_data <- ncdf4::nc_open(filename = file.path(input_path, lake, "openEO.nc"))  
  names(nc_data$var)
  # x, y and t
  x <- ncdf4::ncvar_get(nc = nc_data, varid = "x")
  y <- ncdf4::ncvar_get(nc = nc_data, varid = "y")
  t <- ncdf4::ncvar_get(nc = nc_data, varid = "t")
  t_date <- as.Date(t, origin = "1990-01-01")
  
  # slice by time
  scl_image <- ndtri_image <- list()
  for(t_step in seq_along(t)){
    print(paste0(t_step, " / ", length(t)))
    
    b2 <- ncdf4::ncvar_get(
      nc = nc_data, varid = "B02", 
      start = c(1,1,t_step), 
      count = c(length(x),length(y),1)
    )
    b5 <- ncdf4::ncvar_get(
      nc = nc_data, varid = "B05",  
      start = c(1,1,t_step), 
      count = c(length(x),length(y),1)
    )
    scl <- ncdf4::ncvar_get(
      nc = nc_data, varid = "SCL",
      start = c(1,1,t_step), 
      count = c(length(x),length(y),1)
    )
    
    # negative reflactance need to be set to 0 before index is 
    # calculated, the same is done in the sentinel hub to harmonize
    # the data: https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/
    
    b2t <-  b2
    b2t[b2t < 0] <- 0
    b5t <- b5
    b5t[b5t < 0] <- 0
    sclt <- scl
    # sometimes SCL pixel are NA, in order to calculate the proportion of scenes
    # those values are set to 0, which does not count for any scen from 1 to 11
    sclt[is.na(sclt)] <- 0
    scl_image[[t_step]] <- sclt
    ndtri_image[[t_step]] <- (b5t - b2t) / (b5t + b2t) 
    # remove all cloud and snow pixels 
    ndtri_image[[t_step]][sclt %in% c(2,3, 7:11)] <- NA
    gc(verbose = FALSE)
  }
  
  ncdf4::nc_close(nc = nc_data)
  
  
  # Proportion of all scenes per pixel
  scenes <- lapply(1:11, function(s) {
    Reduce("+", lapply(scl_image, function(image_scene){
      image_scene == s
    }))/ length(scl_image)
  })
  
  # Sum of proportions for clouds and snow or ice
  # 
  clouds_snow_and_topography <- 
    scenes[[7]] + scenes[[8]] + scenes[[9]] + scenes[[10]] + 
    scenes[[11]] + scenes[[3]] + scenes[[2]] 
  # The proportion of water pixels after removing clouds and snow or ice
  water_proportion <- scenes[[6]] / (1 - clouds_snow_and_topography)
  
  plot_water_pixels(
    water_proportion = water_proportion)

  month_of_images <- as.numeric(format(as.Date(t_date), "%m"))
  year_of_images <- as.numeric(format(as.Date(t_date), "%Y"))
  
  months <- 1:12
  
  
  ndtri_monthly <- lapply(
    X = months, 
    FUN = function(x) {
      ndtri_season(
        ndtri_image = ndtri_image, 
        scl_image = scl_image, 
        year = 2018:2024,
        month_of_images = as.numeric(format(as.Date(t_date), "%m")), 
        year_of_images = as.numeric(format(as.Date(t_date), "%Y")), 
        water_scenes_only = TRUE, 
        pixel_quality_threshold = 0.8, 
        first_month = x,
        last_month = x
        )
    }) 
      
  names(ndtri_monthly) <- paste0("month_", months)
  
  
  average_ndtri <- c()
  deviation <- c()
  n_pixel <- c()
  pixel_quality <- c()
  min_water_images <- c()
  modus <- list()
  best_guess <- list()
  for(i in seq_along(ndtri_monthly)){
    NAME <- names(ndtri_monthly)[i]
    mat <- ndtri_monthly[[i]]$NDTrI_season
    wp <- ndtri_monthly[[i]]$water_scene_proportion
    
    n_pixel_year <- sum(!is.na(ndtri_monthly[[i]]$NDTrI_season))
    n_pixel <- c(n_pixel, n_pixel_year)
    
    if(n_pixel_year > 1){
      png(filename = file.path(save_files_here, paste0(NAME, ".png")), 
          width = 10, height = 8, unit = "in", res = 600)
      plot_ndtri_season(
        ndtri_season = mat, x = x, y = y, 
        pal = df_col$color, cuts = df_col$ndtri, year = NAME)
      #plot(poly_new_crs, add = TRUE)
      dev.off()
      
      average_ndtri <- c(average_ndtri, median(mat, na.rm = TRUE))
      
      deviation <- c(deviation, sd(mat, na.rm = TRUE))
      pixel_quality <- c(pixel_quality, ndtri_monthly[[i]]$pixel_quality_threshold)
      min_water_images <- c(min_water_images, ndtri_monthly[[i]]$minimum_water_images)
      
      d <- stats::density(
        mat, 
        bw = "SJ", 
        adjust = 2, 
        na.rm = TRUE)
      
      peak_i <- which(diff(sign(diff(d$y)))==-2) # find maximum (where the sign changes)
      modus[[i]] <- d$x[peak_i[d$y[peak_i] > 2*mean(d$y)]] # relevant only if the peak is higher than average density
      best_guess[[i]] <- if(length(modus[[i]]) > 0L){
        modus[[i]][modus[[i]] == max(modus[[i]])]
      } else {
        NA
      }
      
      png(filename = file.path(save_files_here, paste0(NAME, "_distribution.png")), 
          width = 5, height = 4, unit = "in", res = 300)
      nBreaks <- if(n_pixel_year > 50){
        n_pixel_year/10
      } else {
        5
      }
      
      hist(mat, xlab = "NDTrI per Pixel",  main = NAME,
           breaks = nBreaks, col = "gray80", freq = FALSE, border = NA)
      lines(x = d$x, y = d$y, lwd = 2)
      abline(v = modus[[i]], lty = "dashed")
      abline(v = best_guess[[i]], lty = "dashed", lwd = 2)
      text(x = best_guess[[i]][1], "Selected modus of density function", 
           y = 0, pos = 4, cex = 0.6, xpd = TRUE)
      
      dev.off()
      
    } else {
      average_ndtri <- c(average_ndtri, NA)
      min_water_images <- c(min_water_images, NA)
      pixel_quality <- c(pixel_quality, ndtri_complete[[i]]$pixel_quality_threshold)
      deviation <- c(deviation, NA)
      modus[[i]] <- NA
      best_guess[[i]] <- NA
    }
    
  }
  
  df_out <- data.frame(
    "season" = names(ndtri_monthly),
    "pixel_quality" = pixel_quality,
    "n_Pixels" = n_pixel,
    "minimm_water_images" = min_water_images,
    "Median_NDTrI" = round(average_ndtri,3),
    "Stddev_NDTrI" = round(deviation,4),
    "Modus_NDTrI" = round(unlist(best_guess),3),
    "nModus_info" = sapply(modus, length))
  
  png(filename = file.path(save_files_here, "monthly-mean.png"), 
      width = 6, height = 5, unit = "in", res = 300)
  plot(x = 1:12, y = df_out$Modus_NDTrI, type = "b", lwd = 2, lty = "dashed", 
       pch = 19, cex = 1.5, xaxs = "i", main = "Mean monthly NDTrI (Data from 2018 to 2024)",
       ylab = "NDTrI", xlab = "Month", ylim = c(-0.6, 0.1))
  dev.off()
  
  write.table(x = df_out, 
              file = file.path(save_files_here, "monthly_data.csv"), 
              sep = ";", dec = ".", row.names = FALSE)
  

}

# functions
index_wise_average <- function(list_of_mats, na.rm = FALSE){
  if(na.rm){
    no_na_values <- 
      length(list_of_mats) - Reduce("+",lapply(list_of_mats, is.na))
    list_of_mats <- lapply(list_of_mats, function(x){
      x[is.na(x)] <- 0
      x
    })
  } else {
    no_na_values <- length(list_of_mats)
  }
  
  Reduce("+", list_of_mats) / no_na_values
}

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
    col = pal, main = year)
}

ndtri_season <- function(
    year, ndtri_image, scl_image, month_of_images, year_of_images, 
    water_scenes_only = TRUE, pixel_quality_threshold = 0.8,
    first_month = 4L, last_month = 10L
){
  
  used_for_ndtri <- month_of_images >= first_month & 
    month_of_images <= last_month & 
    year_of_images %in% year
  
  ndtri_seasonal_image <- ndtri_image[used_for_ndtri]
  scl_seasonal_image <- scl_image[used_for_ndtri]
  
  # Proportion of all scenes per pixel
  scenes <- lapply(1:11, function(s) {
    Reduce("+", lapply(scl_seasonal_image, function(image_scene){
      image_scene == s
    }))/ length(scl_seasonal_image)
  })
  
  # Sum of proportions for clouds and snow or ice
  clouds_snow_and_topography <- 
    scenes[[7]] + scenes[[8]] + scenes[[9]] + scenes[[10]] + scenes[[11]] + scenes[[3]] + scenes[[2]]
  
  # The proportion of water pixels after removing clouds and snow or ice
  water_proportion <- scenes[[6]] / (1 - clouds_snow_and_topography)
  
  if(water_scenes_only){
    ndtri_seasonal_image <- lapply(seq_along(ndtri_seasonal_image), function(i){
      ndtri_seasonal_image[[i]][scl_seasonal_image[[i]] != 6] <- NA
      ndtri_seasonal_image[[i]]
    })
  }
  
  ndtri <- index_wise_average(
    list_of_mats = ndtri_seasonal_image, 
    na.rm = TRUE
  )
  
  # remove all uncertain pixels
  use_pixel <- water_proportion >= pixel_quality_threshold

  ndtri[!use_pixel] <- NA
  
  list("NDTrI_season" = ndtri,
       "water_scene_proportion" = water_proportion,
       "scene_proportions" = scenes,
       "pixel_quality_threshold" = pixel_quality_threshold,
       "minimum_water_images" = floor(pixel_quality_threshold * sum(used_for_ndtri))
  )
}

# Color palette for trophic state
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
