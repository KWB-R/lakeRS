library(ncdf4)
library(raster)

path <- 
  file.path(
    "Y:/SUW_Department/Projects/AD4GD/Exchange/01_data/01_input/satellite_data",
    "openeo/netcdf")

lake <- dir(path)[9]



if(FALSE){
  nc_data <- ncdf4::nc_open(filename = file.path(path, lake, "openEO.nc"))  
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
  
  # slice by time
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
    # those values are set to 0, which does not count for any scen from 1 to 11
    sclt[is.na(sclt)] <- 0
    scl_image[[t_step]] <- sclt
    ndtri_image[[t_step]] <- (b5t - b2t) / (b5t + b2t) 
    # remove all cloud and snow pixels 
    ndtri_image[[t_step]][sclt %in% 7:11] <- NA
  }
  
  # Proportion of all scenes per pixel
  scenes <- lapply(1:11, function(s) {
    Reduce("+", lapply(scl_image, function(image_scene){
      image_scene == s
    }))/ length(scl_image)
  })
  
  # Sum of proportions for clouds and snow or ice
  clouds_and_snow <- 
    scenes[[7]] + scenes[[8]] + scenes[[9]] + scenes[[10]] + scenes[[11]]
  
  # The proportion of water pixels after removing clouds and snow or ice
  water_proportion <- scenes[[6]] / (1 - clouds_and_snow)

  
  # Plot proportions of water pixel
  # dev.new()
  # plot_water_pixels(
  #   water_proportion = water_proportion)

  # ----------------------------------------------------------------------------
  month_of_images <- as.numeric(format(as.Date(t_date), "%m"))
  year_of_images <- as.numeric(format(as.Date(t_date), "%Y"))

  
  # plot all pixels, no mater if water or not
  # {
  #   ndtri_2018 <- ndtri_season(
  #     year = 2018, 
  #     ndtri_image = ndtri_image, 
  #     scl_image = scl_image, 
  #     month_of_images = as.numeric(format(as.Date(t_date), "%m")), 
  #     year_of_images = as.numeric(format(as.Date(t_date), "%Y")), 
  #     water_scenes_only = FALSE, 
  #     pixel_quality_threshold = 0)
  #   r <- raster::raster(
  #     t(ndtri_2018), 
  #     xmn=min(x), 
  #     xmx=max(x), 
  #     ymn=min(y), 
  #     ymx=max(y))
  #   plot(r, xlim = c(min(x), max(x)), ylim = c(min(y), max(y)))
  # }
  # 
  # # plot water scene only
  # {
  #   ndtri_2018 <- ndtri_season(
  #     year = 2018, 
  #     ndtri_image = ndtri_image, 
  #     scl_image = scl_image, 
  #     month_of_images = as.numeric(format(as.Date(t_date), "%m")), 
  #     year_of_images = as.numeric(format(as.Date(t_date), "%Y")), 
  #     water_scenes_only = TRUE, 
  #     pixel_quality_threshold = 0)
  #   r <- raster::raster(
  #     t(ndtri_2018), 
  #     xmn=min(x), 
  #     xmx=max(x), 
  #     ymn=min(y), 
  #     ymx=max(y))
  #   plot(r, xlim = c(min(x), max(x)), ylim = c(min(y), max(y)))
  # }
  # 
  # plot water scene only and use only high water probability pixels
  # {
  #   ndtri_2018 <- ndtri_season(
  #     year = 2018, 
  #     ndtri_image = ndtri_image, 
  #     scl_image = scl_image, 
  #     month_of_images = as.numeric(format(as.Date(t_date), "%m")), 
  #     year_of_images = as.numeric(format(as.Date(t_date), "%Y")), 
  #     water_scenes_only = TRUE, 
  #     pixel_quality_threshold = 0.8)
  #   r <- raster::raster(
  #     t(ndtri_2018), 
  #     xmn=min(x), 
  #     xmx=max(x), 
  #     ymn=min(y), 
  #     ymx=max(y))
  #   cuts <- df_col$ndtri
  #   pal <- df_col$color
  #   plot(r, xlim = c(min(x), max(x)), ylim = c(min(y), max(y)), breaks = cuts, col = pal)
  # }

  pixel_quality_threshold <- 0.8
  years <- 2018:2024
  ndtri_complete <- lapply(
    X = years, 
    FUN = ndtri_season, 
    ndtri_image = ndtri_image, 
    scl_image = scl_image, 
    month_of_images = as.numeric(format(as.Date(t_date), "%m")), 
    year_of_images = as.numeric(format(as.Date(t_date), "%Y")), 
    water_scenes_only = TRUE, 
    pixel_quality_threshold = pixel_quality_threshold)
  names(ndtri_complete) <- paste0("Year_", years)
  
  average_ndtri <- c()
  deviation <- c()
  n_pixel <- c()
  modus <- list()
  for(i in seq_along(ndtri_complete)){
    NAME <- names(ndtri_complete)[i]
    mat <- ndtri_complete[[i]]$NDTrI_season
    wp <- ndtri_complete[[i]]$water_scene_proportion
    
    png(filename = file.path(path, lake, paste0(NAME, ".png")), 
        width = 10, height = 8, unit = "in", res = 600)
    plot_ndtri_season(
      ndtri_season = mat, x = x, y = y, 
      pal = df_col$color, cuts = df_col$ndtri, year = NAME)
    dev.off()
    
    average_ndtri <- c(average_ndtri, median(mat, na.rm = TRUE))
    n_pixel <- c(n_pixel, sum(wp >= pixel_quality_threshold, na.rm = TRUE))
    deviation <- c(deviation, sd(mat, na.rm = TRUE))
    
    d <- stats::density(mat, bw = "SJ", adjust = 5, na.rm = TRUE, n = 512)
    peak_i <- which(diff(sign(diff(d$y)))==-2) # find maximum (where the sign changes)
    modus[[i]] <- d$x[peak_i[d$y[peak_i] > mean(d$y)]] # relevant only if the peak is higher than average density
    
  }
  
  data.frame(
    "season" = names(ndtri_complete),
    "Median_NDTrI" = average_ndtri,
    "Stddev_NDTrI" = deviation,
    "n_Pixels" = n_pixel,
    "Modus_NDTrI" = ifelse(
      test = rep(length(unlist(modus)) == length(n_pixel), length(n_pixel)), 
      yes = unlist(modus), 
      no = paste0("n_mod = ", sapply(modus,length))))
  
  as.data.frame(modus)
  modus[[1]] <- c(modus[[1]], 0.1)
  d <- stats::density(mat, bw = "SJ", adjust = 5, na.rm = TRUE, n = 512)
  plot(d)
  peakx <- d$x[which(diff(sign(diff(d$y)))==-2 & # miminum or maximum (where the sign changes)
              d$y > median(d$y))] # relevant only if the peak is higher than average density
  
  
  
  
  
  # Save a new netCDF or extend the existing one
  ncdim_def(name = )
  
  
  
  new_var <- ncdf4::ncvar_def(
    name = "NDTrI_season", 
    units = "m", 
    dim = nc_data$dim, 
    longname = "Normalized difference trophic index for a whole season")
  
  ncdf4::ncvar_add(nc = nc_data, )
  nc_data$dim
  ncdf4::nc_create(
    filename = file.path(
      "Y:/SUW_Department/Projects/AD4GD/Exchange", 
      "01_data/03_output/netCDF_ndti", "lietzensee_ndti_2024.nc"), 
    vars = new_var)
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
    year_of_images == year
  ndtri_seasonal_image <- ndtri_image[used_for_ndtri]
  scl_seasonal_image <- scl_image[used_for_ndtri]
  
  # Proportion of all scenes per pixel
  scenes <- lapply(1:11, function(s) {
    Reduce("+", lapply(scl_image, function(image_scene){
      image_scene == s
    }))/ length(scl_image)
  })
  
  # Sum of proportions for clouds and snow or ice
  clouds_and_snow <- 
    scenes[[7]] + scenes[[8]] + scenes[[9]] + scenes[[10]] + scenes[[11]]
  
  # The proportion of water pixels after removing clouds and snow or ice
  water_proportion <- scenes[[6]] / (1 - clouds_and_snow)
  
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
       "scene_proportions" = scenes)
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
