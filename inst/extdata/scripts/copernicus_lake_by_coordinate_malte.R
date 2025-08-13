# install.packages("openeo")
library(openeo)

if(FALSE){
  # gee <- connect(host = "https://earthengine.openeo.org")
  # active_connection()
  eodc <- connect(host = "https://openeo.dataspace.copernicus.eu/openeo/1.2")
  available_collections <- list_collections()
  describe_collection(available_collections$SENTINEL2_L2A)
  describe_collection(available_collections$SENTINEL2_L1C)
  
  available_processes <- list_processes()
  "filter_spatial" %in% names(available_processes) # filter spatial is in there
  
  # magic login function (opens the browser to log in with copernicus dataspace credentials)
  login()
  
  p <- processes()
  
  lake_name <- "Brandiser Badess"
  # One point only Does not work --> Empty output
  lat <- 13.168516
  lon <- 51.790693
  
  # enlarge point to rectangle (library geosprhere is needed)
  meters <- 20
  top <- geosphere::destPoint(p = c(lat, lon),b = 0, d = meters)[2] # to the top
  bottom <- geosphere::destPoint(p = c(lat, lon),b = 180, d = meters)[2] # to the bottom
  left <- geosphere::destPoint(p = c(lat, lon),b = -90, d = meters)[1] # to the left
  right <- geosphere::destPoint(p = c(lat, lon),b = 90, d = meters)[1] # to the right
  
  lake_point <- p$load_collection(
    "id" = "SENTINEL2_L2A",
    "spatial_extent" = list(
      "crs" = 4326, 
      "east" =  right, 
      "north" =  top, 
      "south" = bottom,
      "west" = left),
    "temporal_extent" = list("2018-01-01", "2024-01-01"), #  '2024' is short for  "2024-01-01", left side included, right side excluded
    "bands" = list("B05", "B02", "SCL"), # NULL for all bands
    "properties" = NULL
  )
  
  
  # all available formats
  formats = list_file_formats()
  
  result_netcdf = p$save_result(
    data = lake_point, 
    format = formats$output$netCDF # CSV is not supported for not reduced data
  )
  
  # This does not execute on the back-end, we have just created the process chain
  # further processes like aggregate or reduce can be applied prior to the job
  job_title <- lake_name
  
  # Execute it on the back-end ("batch-job")
  job = create_job(
    graph = result_netcdf,
    title = job_title)
  # sends all necessary information to the back-end and creates a new job, 
  # which gets returned. After this, the job is just created, but has not started 
  # the execution at the back-end yet.
  
  start_job(job = job)
  
  jobs <- list_jobs()
  
  last_job <- jobs[[1]]
  describe_job(last_job)
  
  download_results(
    job = last_job, 
    folder = file.path(
      "C:/Users/mzamzo/Documents/AD4GD/remote_sensing/input", job_title) 
  )
  
  # load and process data
  nc_data <- ncdf4::nc_open(
    filename = file.path(
      "C:/Users/mzamzo/Documents/AD4GD/remote_sensing/input", job_title,  "openEO.nc"))  
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
    ndtri_image[[t_step]][sclt %in% c(2,3, 7:11)] <- NA
  }
  
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
  
  pixel_quality_threshold <- 0.8
  years <- 2018:2023
  
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
  pixel_quality <- c()
  min_water_images <- c()
  modus <- list()
  best_guess <- list()
  for(i in seq_along(ndtri_complete)){
    NAME <- names(ndtri_complete)[i]
    mat <- ndtri_complete[[i]]$NDTrI_season
    wp <- ndtri_complete[[i]]$water_scene_proportion
    
    n_pixel_year <- sum(!is.na(ndtri_complete[[i]]$NDTrI_season))
    n_pixel <- c(n_pixel, n_pixel_year)
    
    # plot_ndtri_season(
    #   ndtri_season = mat, x = x, y = y, 
    #   pal = df_col$color, cuts = df_col$ndtri, year = NAME)
    # 
    average_ndtri <- c(average_ndtri, median(mat, na.rm = TRUE))
    
    deviation <- c(deviation, sd(mat, na.rm = TRUE))
    pixel_quality <- c(pixel_quality, ndtri_complete[[i]]$pixel_quality_threshold)
    min_water_images <- c(min_water_images, ndtri_complete[[i]]$minimum_water_images)
    
    d <- stats::density(
      mat, 
      bw = "SJ", 
      adjust = 2, 
      na.rm = TRUE)
    
    peak_i <- which(diff(sign(diff(d$y)))==-2) # find maximum (where the sign changes)
    modus[[i]] <- d$x[peak_i[d$y[peak_i] > 1.5 * mean(d$y)]] # relevant only if the peak is higher than average density
    best_guess[[i]] <- if(length(modus[[i]]) > 0L){
      modus[[i]][modus[[i]] == max(modus[[i]])]
    } else {
      NA
    }
    
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
  }
  
  df_out <- data.frame(
    "season" = names(ndtri_complete),
    "pixel_quality" = pixel_quality,
    "n_Pixels" = n_pixel,
    "minimm_water_images" = min_water_images,
    "Median_NDTrI" = round(average_ndtri,3),
    "Stddev_NDTrI" = round(deviation,4),
    "Modus_NDTrI" = round(unlist(best_guess),3),
    "nModus_info" = sapply(modus, length))
  
  new_row <- data.frame(
    "name" = lake_name, 
    "id" = NA,
    "pixel_lat" = lat,
    "pixel_lon" = lon,
    "2018" = df_out$Median_NDTrI[df_out$season == "Year_2018"],
    "2019" = df_out$Median_NDTrI[df_out$season == "Year_2019"],
    "2020" = df_out$Median_NDTrI[df_out$season == "Year_2020"],
    "2021" = df_out$Median_NDTrI[df_out$season == "Year_2021"],
    "2022" = df_out$Median_NDTrI[df_out$season == "Year_2022"],
    "2023" = df_out$Median_NDTrI[df_out$season == "Year_2023"]) 
  colnames(new_row) <- gsub(pattern = "X", replacement = "", x = colnames(new_row))
  
  old_rows <- read.csv(file = file.path(
    "C:/Users/mzamzo/Documents/AD4GD/remote_sensing/input", 
    "yearly_spread.csv")
    , sep = ";", dec = ".")
  colnames(old_rows) <- gsub(pattern = "X", replacement = "", x = colnames(old_rows))
  
 
  
  yearly_spread <- rbind(old_rows, new_row)
  
  write.table(x = yearly_spread, 
              file = file.path(
                "C:/Users/mzamzo/Documents/AD4GD/remote_sensing/input", 
                "yearly_spread.csv"), 
              sep = ";", dec = ".", row.names = FALSE)
  
  # NDTrI Results
  ndtri_status <- get_ndtri_status(yearly_spread = yearly_spread, n_years = 3)
  ndtri_trend <- get_ndtri_trend(yearly_spread = yearly_spread, short_term = 3, long_term = 7)
  
  EO_assessment_ndtri <- do.call(cbind, list(
    yearly_spread, 
    ndtri_status, 
    ndtri_trend)
  )
  
  
  # Class results
  ndtri_classes <- get_ndtri_classes(yearly_spread = yearly_spread, n_classes = 10)
  
  df_class <- EO_assessment_class <- do.call(cbind, list(
    yearly_spread, 
    ndtri_classes$yearly_classes,  
    ndtri_classes$yearly_colors))
  
  
  plot_ndtri_status(EO_assessment = EO_assessment_ndtri, lake_names = lake_name)
  
  lakes <- c("Großer Wummsee","Braminsee",  "Bernsteinsee", "Körbaer Teich")
  which(df_class$name %in% lakes)
  # set.seed(1)
  rows_to_plot <- which(df_class$name %in% lakes)
  # c(sample(1:nrow(df_class), 15),  which(df_class$name == "Großer Stechlinsee"))
  
  dev.new(noRStudioGD = TRUE, height = 8, width = 5)
  plot_ndtri_classes(
    EO_assessment = df_class, 
    rows_to_plot = rows_to_plot
  )
  
}

plot_ndtri_status <- function(EO_assessment, lake_names = NULL, lake_ids = NULL, row_numbers = NULL){
  r <- c()
  if(!is.null(row_numbers)){
    r <- c(r, row_numbers)
  }
  r <- c(
    r, 
    get_rows_by_info(
      df = EO_assessment, 
      name_column = "name", 
      id_column = "id", 
      lake_names = lake_names, 
      lake_ids = lake_ids)
  )
  df_plot <- EO_assessment[r,]
  status_columns <- grep(pattern = "[0-9]_status$", x = colnames(df_plot))
  single_year_columns <- grep(pattern = "[0-9]$", colnames(df_plot))
  
  
  n_status_years <- length(status_columns)
  n_single_years <- length(single_year_columns)
  
  stats <- sapply(X = EO_assessment[,single_year_columns], 
                  quantile, 
                  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1))
  
  dev.new(noRStudioGD = TRUE, width = 6, height = 4)
  
  layout(mat = matrix(data = c(1,1,1,1,2,2,3,4), 
                      nrow = 2, ncol = 4, byrow = TRUE), 
         heights = c(0.2, 1))
  par(mar = c(0,0,0,0))
  plot(0,0,type ="n", xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n")
  text(x = 0, y = 0, df_plot$name, cex = 2)
  
  par(mar = c(4.1, 4.1, 1.1, 1.1))
  plot(x = 1:n_single_years, y = stats[5,], type = "n", 
       ylim =range(stats), ylab = "NDTrI", xaxt = "n", xlab = "")
  polygon(x = c(1:n_single_years, n_single_years:1), 
          y = c(stats[1,], rev(stats[7,])), 
          col = rgb(208, 207, 253, 150, maxColorValue = 255, ), 
          border = NA)
  polygon(x = c(1:n_single_years, n_single_years:1), 
          y = c(stats[2,], rev(stats[6,])), 
          col = rgb(161, 160, 251, 100, maxColorValue = 255), 
          border = NA)   
  polygon(x = c(1:n_single_years, n_single_years:1), 
          y = c(stats[3,], rev(stats[5,])), 
          col = rgb(115, 112, 248, 50, maxColorValue = 255), 
          border = NA)
  lines(x = 1:n_single_years, y = stats[4,], col = rgb(0,3,226, 50,maxColorValue = 255), lwd = 2)
  
  points(x = 1:n_single_years,
         y = df_plot[,single_year_columns], 
         pch = 19)
  lines(x = 1:n_status_years + n_single_years - n_status_years, 
        y = df_plot[,status_columns], 
        lwd = 1, lty = "dotted")
  
  axis(side = 1, 
       at = 1:n_single_years, 
       labels = colnames(df_plot)[single_year_columns], 
       las = 2)
  
  plot(x = 0, y = 0, ylab = "Langzeittrend", type = "n", xlab = "", xaxt = "n",
       ylim = range(EO_assessment$trend_long))
  abline(h = 0, lty = "dashed")
  points(x = 0, y = df_plot$trend_long, pch = 19, cex = 2)
  lines(x = c(0,0), 
        y = df_plot$trend_long + 1.96 * c(df_plot$error_long,-df_plot$error_long), 
        lwd = 3)
  
  plot(x = 0, y = 0, ylab = "Kurzzeittrend", type = "n", xlab = "", xaxt = "n",
       ylim = range(EO_assessment$trend_short))
  abline(h = 0, lty = "dashed")
  points(x = 0, y = df_plot$trend_short, pch = 19, cex = 2)
  lines(x = c(0,0), 
        y = df_plot$trend_short + 1.96 * c(df_plot$error_short,-df_plot$error_short), 
        lwd = 3)
}

plot_ndtri_classes <- function(EO_assessment, rows_to_plot){
  df_plot <-  EO_assessment[rows_to_plot,]
  
  class_cols <- grep(pattern = "_class$", colnames(df_plot))
  color_cols <- grep(pattern = "_color$", colnames(df_plot))
  years <- substr(colnames(df_plot)[class_cols], 1, 4)
  n_years <- length(class_cols)
  n_lakes <- nrow(df_plot)
  par(mar = c(4.1, 10.1, 1.1,1.1))
  plot(x = 0, y = 0, 
       xlim = c(0,n_years) + 0.5, 
       ylim = c(0,n_lakes) + 0.5, xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  axis(side = 1, at = 1:n_years, labels = years, las = 2, tick = FALSE)
  axis(side = 2, at = rev(1:n_lakes), 
       labels = paste0(df_plot$name, "\n" , "(", df_plot$id, ")"), las = 1)
  
  df_reverse <- df_plot[nrow(df_plot):1,]
  for(i in 1:n_lakes){
    for(j in 1:n_years){
      rect(xleft = j - 0.5, xright = j + 0.5, ybottom = i - 0.5, ytop = i + 0.5, 
           col = df_reverse[i,color_cols[j]])
      text(x = j, y = i, labels = df_reverse[i,class_cols[j]], font = 2)
    }
  }
  
}

get_rows_by_info <- function(df, name_column = NULL, id_column = NULL, lake_names = NULL, lake_ids = NULL){
  r <- c()
  if(!is.null(lake_names)){
    missing <- which(!(lake_names %in% df[[name_column]]))
    if(length(missing) > 0){
      warning(paste("No lake/lakes called", lake_names[missing], "in data frame."))
    } 
    r <- c(r, which(df[[name_column]] %in% lake_names))
  } 
  if(!is.null(lake_ids)){
    missing <- which(!(lake_ids %in% df[[id_column]]))
    if(length(missing) > 0){
      warning(paste("No lake ID/IDs", lake_names[missing], "in data frame."))
    } 
    r <- c(r, which(df[[name_column]] %in% lake_names))
  }
  r 
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
  # if(sum(use_pixel) < 5){
  #   pixel_quality_threshold <- 0.3
  #   use_pixel <- water_proportion >= pixel_quality_threshold
  # }
  ndtri[!use_pixel] <- NA
  
  list("NDTrI_season" = ndtri,
       "water_scene_proportion" = water_proportion,
       "scene_proportions" = scenes,
       "pixel_quality_threshold" = pixel_quality_threshold,
       "minimum_water_images" = floor(pixel_quality_threshold * sum(used_for_ndtri))
  )
}

get_ndtri_status <- function(yearly_spread, n_years = 3){
  first_year_column <- min(grep(pattern = "^[0-9]",colnames(yearly_spread)))
  last_year_column <- ncol(yearly_spread)
  
  df_out <- sapply(first_year_column:(last_year_column - n_years + 1), function(i){
    x <- yearly_spread[,i:(i + n_years - 1)]
    apply(x, 1, mean)
  })
  colnames(df_out) <- 
    paste0(colnames(yearly_spread)[(first_year_column + n_years - 1):last_year_column], "_status")
  cbind(df_out, "years_status" =  c(n_years))
}

get_ndtri_trend <- function(yearly_spread = yearly_spread, short_term = 3, long_term = 7){
  single_year_columns <- grep(pattern = "^[0-9]", 
                              x = colnames(yearly_spread))
  
  mat <- as.matrix(yearly_spread[,single_year_columns])
  overall_median <- median(mat)
  yearly_medians <- apply(mat, 2, median)
  yearly_shift <- overall_median - yearly_medians
  
  if(short_term > ncol(mat)){
    short_term <- ncol(mat)
  }
  if(long_term > ncol(mat)){
    long_term <- ncol(mat)
  }
  
  short_term_columns <- (ncol(mat) - short_term:1 + 1)
  long_term_columns <- (ncol(mat) - long_term:1 + 1)
  
  short_term_mat <- mat[,short_term_columns]
  long_term_mat <- mat[,long_term_columns]
  
  
  t(sapply(1:nrow(mat), function(i){
    short_term_df <- data.frame(
      "Year" = colnames(short_term_mat),
      "NDTrI" = short_term_mat[i,],
      "NDTrI_adjusted" = short_term_mat[i,] + yearly_shift[short_term_columns],
      "years_passed" = -(short_term - 1):0
    )
    long_term_df <- data.frame(
      "Year" = colnames(long_term_mat),
      "NDTrI" = long_term_mat[i,],
      "NDTrI_adjusted" = long_term_mat[i,] + yearly_shift[long_term_columns],
      "years_passed" = -(long_term - 1):0
    )
    
    st <- summary(lm(NDTrI_adjusted ~ years_passed, data = short_term_df))
    lt <- summary(lm(NDTrI_adjusted  ~ years_passed, data = long_term_df))
    
    c("trend_short" = st$coefficients[2,1],
      "error_short" = st$coefficients[2,2],
      "years_short" = length(short_term_columns),
      "trend_long" = lt$coefficients[2,1],
      "error_long" = lt$coefficients[2,2],
      "years_long" = length(long_term_columns))
  }))
}

get_ndtri_classes <- function(yearly_spread, n_classes = 10){
  
  color_table <- data.frame(
    "class" = 1:10,
    "color" = c(
      rgb(94,79,162, maxColorValue = 255),
      rgb(50,136,189, maxColorValue = 255),
      rgb(102,194,164, maxColorValue = 255),
      rgb(171,221,164, maxColorValue = 255),
      rgb(230,245,152, maxColorValue = 255),
      rgb(254,224,139, maxColorValue = 255),
      rgb(253,174,94, maxColorValue = 255),
      rgb(244,109,67, maxColorValue = 255),
      rgb(213,62,79, maxColorValue = 255),
      rgb(158,1,66, maxColorValue = 255)
    ))
  
  last_year_column <- ncol(yearly_spread)
  
  yearly_classes <- sapply(grep(pattern = "^[0-9][0-9][0-9][0-9]", colnames(yearly_spread)), function(i){
    year_i <- yearly_spread[,i]
    breaks <- c(-1,
                seq(quantile(year_i, probs = 0.05), 
                    quantile(year_i, probs = 0.95), length.out = n_classes - 1), 
                1)
    cut(x = year_i, breaks = breaks, labels = color_table$class, include.lowest = TRUE)
  })
  colnames(yearly_classes) <- 
    paste0(colnames(yearly_spread)[grep(pattern = "^[0-9][0-9][0-9][0-9]", colnames(yearly_spread))], "_class")
  
  yearly_colors <- sapply(grep(pattern = "^[0-9][0-9][0-9][0-9]", colnames(yearly_spread)), function(i){
    year_i <- yearly_spread[,i]
    breaks <- c(-1,
                seq(quantile(year_i, probs = 0.05), 
                    quantile(year_i, probs = 0.95), length.out = n_classes - 1), 
                1)
    cut(x = year_i, breaks = breaks, labels = color_table$color, include.lowest = TRUE)
  })
  colnames(yearly_colors) <- 
    paste0(colnames(yearly_spread)[grep(pattern = "^[0-9][0-9][0-9][0-9]", colnames(yearly_spread))], "_color")
  
  list("yearly_classes" = yearly_classes, "yearly_colors" = yearly_colors)
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





