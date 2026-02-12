#' The moving average dynamic of an RS index
#' 
#' The dynamic can include many years, all sorted by the day of the year.
#' The aggregation per pixel is a moving average.
#' 
#' @param ncImage the data list created by [ndi_per_image()],
#' which must contain the SCL layer if water_scenes_only is TRUE.
#' @param t_date A vector of the dates of images. This is part of the output
#' of [load_netcdf()], with the list name "t_date".
#' @param years Numeric vector of all the years to be included
#' @param water_scenes_only By default only pixels classified as water scene are 
#' used for the yearly average per pixel. If False, all values are included
#' except for cloud and snow scenes.
#' @param days_around_ma The days around the moving average day (i.e. 10  means
#' 10 days before and ten days after the actual day are used for averaging). If
#' NULL the number of days around the moving average is derived from the 
#' median available values per pixel.
#' @param threshold The mininum number of valid values for one pixel to be 
#' processed (the higher the days around moving averages are, the lower this
#' threshold can be)
#' @param maxPixels Maximum number of pixels (randomly chosen from available pixels).
#' If Inf, all pixels will be analysed.
#' @param lakeInfo Character vector of length 2 specifying the Name and the ID
#' of the lake. This is not needed for any calculation but is important to 
#' identify the lake.
#' @param maxDataPoints The number of datapoints to be part of one matrix
#' for moving average calculation. The default (5E+06) corresponds to 5000 pixels
#' in 1000 images. 
#' 
#' @details
#' A moving average is calculated for a day if at least two different images 
#' are available. If the number of images is not sufficient the days_around_ma
#' need to be increased and the results will become smoother.
#' 
#' 
#' 
#' @export
#' 
dynamic_per_pixel <- function(
    ncImage, t_date, years, water_scenes_only = TRUE, days_around_ma = NULL, 
    maxPixels = 1000, threshold = NULL, lakeInfo = c("", ""), maxDataPoints = 5000000
){
  output <- list()
  output[["lakeInfo"]] <- lakeInfo
  
  imageDOY <- as.numeric(format(t_date, "%j"))
  imageYear <- as.numeric(format(t_date, "%Y"))
  
  yearFilter <- imageYear %in% years
  
  indexImages <- ncImage$RSindex[yearFilter]
  imageDOY <- imageDOY[yearFilter]
  sclImages <- ncImage$SCL[yearFilter]
  
  d <- dim(indexImages[[1]])
  
  cat("Image data is filtered for SCL categories pixel by pixel ... \n")
  
  for(i in seq_along(indexImages)){
    indexImages[[i]] <- if(water_scenes_only){
      scl_filter(
        indexImage = indexImages[[i]], 
        sclImage = sclImages[[i]], 
        bands = 6, 
        invert = TRUE)
    } else {
      scl_filter(
        indexImage = indexImages[[i]], 
        sclImage = sclImages[[i]], 
        bands = c(8:11))    
    }
  }
  
  t_doy <- imageDOY[order(imageDOY)]
  indexImages_doy <- indexImages[order(imageDOY)]
  
  cat("Data is reshaped from spatial image data to timeseries per pixel ... \n")
  ts <- matrix(
    data = unlist(indexImages_doy), 
    nrow = d[1] * d[2], 
    ncol = length(indexImages_doy)
  )
  
  valid_values <- apply(ts, 1 , function(p_ts){sum(!is.na(p_ts))})
  med_values <- median(valid_values[valid_values > 0])
  if(is.null(days_around_ma)){
    days_around_ma <- ceiling(365 / med_values * 10)
  }
  
  if(is.null(threshold)){
    threshold <- 365 / days_around_ma * 10 
  }
  pixel_selection <- valid_values > threshold
  
  if(sum(pixel_selection) == 0){
    return(
      list("moving_averages" = NULL,
           "raster_location" = data.frame(
             "pixel" = NA,
             "i_col" = 0,
             "i_row" = 0,
             "valid_values" = 0)
      )
    )
  }
  
  pixel_selection_i <- which(pixel_selection)
  nAvailable <- length(pixel_selection_i)
  if(nAvailable > maxPixels){
    pixel_selection_i <- sample(pixel_selection_i, maxPixels)
    nAvailable <- length(pixel_selection_i)
  }
  
  ts_start <- ts[pixel_selection_i,]
  rm(ts)
  equal_size_nRow <- ceiling(maxDataPoints / dim(ts_start)[2])
  nm <- ceiling(dim(ts_start)[1]/equal_size_nRow)
  if(prod(dim(ts_start)) > maxDataPoints){
    cat(paste0(paste0(
      "Due to large matrix size, data is splitted into ", 
      nm+1, " small matrices ... \n")
    ))
    ts_parts <- list()
    i <- 1
    while(nrow(ts_start) > equal_size_nRow){
      ts_parts[[i]] <- ts_start[1:equal_size_nRow,]
      ts_start <- ts_start[-(1:equal_size_nRow),]
      i <- i + 1
    }
    ts_parts[[i]] <- ts_start[1:nrow(ts_start),]
    ts_start <- ts_parts
    rm(ts_parts)
    gc()
  }  else {
    ts_start <- list(ts_start)
  }
  
  cat(paste0(
    "Calculate moving averages of ", nAvailable, " pixel timesseries ... \n")
  )
  ipa <- images_per_ma(
    t_doy = t_doy,
    days_around_ma = days_around_ma
  )

  cat(paste0("Processing ", nm+1, " matrices ... \n"))
  df_out <- lapply(ts_start, function(x){
    cat(" | matrix done")
    lake_output <- lapply(seq_along(ipa), function(i){
      imagesUsed <- which(ipa[[i]])
      if(length(imagesUsed) > 1L){ # at least 2 images need to be available
        rowMeans(
          x = matrix(
            data = x[,which(ipa[[i]])], 
            nrow = nrow(x), 
            ncol = length(imagesUsed)), 
          na.rm = TRUE)
      } else {
        rep(NA, nrow(x))
      }
    })
    do.call(rbind, lake_output)
  })
  cat(" | all matrixes done \n")
  
  if(length(df_out) > 1L){
    df_out <- do.call(cbind, df_out)
  } else {
    df_out <- df_out[[1]]
  }
  
  # no extrapolation of long periods (relative to days around moving average) 
  # without data
  missingDOY <- !(1:365 %in% t_doy)
  binaryChar <- paste0(as.character(as.numeric(missingDOY)), collapse = "")
  binaryChar <- gsub(pattern = "10", replacement = "100", binaryChar)
  daysOfPeriod <- dayRepeats <- nchar(strsplit(binaryChar,split = "0")[[1]])
  dayRepeats[dayRepeats == 0] <- 1 
  t1 <- daysOfPeriod[1]
  t365 <- daysOfPeriod[length(daysOfPeriod)]
  if(t1 > 0 & t365 > 0){
    daysOfPeriod[1] <- daysOfPeriod[length(daysOfPeriod)] <- t1 + t365
  }
  periodPerDay <- rep(daysOfPeriod, dayRepeats)
  removeValues <- (1:365)[periodPerDay > days_around_ma / 4]
 

  if(length(removeValues) > 1){
    df_out[removeValues,] <- NA 
  }
 
  colnames(df_out) <- paste0("p_", 1:ncol(df_out))
  df_out <- cbind("doy" = 1:365, df_out)
 
  i_col <- ceiling(pixel_selection_i/ d[1])
  i_row <- pixel_selection_i - d[1] * (i_col - 1)
  
  list("moving_averages" = df_out,
       "raster_location" = data.frame(
         "pixel" = colnames(df_out)[2:ncol(df_out)],
         "i_col" = i_col,
         "i_row" = i_row,
         "valid_values" = valid_values[pixel_selection_i]),
       "meta" = c("days_around_ma" = days_around_ma, 
                  "threshold_value" = threshold)
  )
}

#' Subset of all images to be used for moving averages for each day
#' 
#' @param t_doy Numeric vector of the day of the year according to ts_pixel 
#' (same length and order)
#' @param days_around_ma The days around the moving average day (i.e. 10  means
#' 10 days before and ten days after the actual day are used for averaging)
#' 
images_per_ma <- function(t_doy, days_around_ma){
  lapply(1:365, function(doy){
    d_range <- doy + c(-days_around_ma, days_around_ma)
    d_range[d_range < 1] <- d_range[d_range < 1] + 365
    d_range[d_range > 365] <- d_range[d_range > 365] - 365
    
    if(d_range[2] > d_range[1]){
      t_doy > d_range[1] & t_doy < d_range[2]
    } else {
      t_doy > d_range[1] | t_doy < d_range[2]
    }
  })
}

#' Set values NA by SCL
#' 
#' @param indexImage,sclImage Index and SCL layer of the image
#' @param bands Numeric vector specifiyng the SCL categories to filtered for
#' @param invert If TRUE the filtering is inverted -> specefied bands are removed
#' 
scl_filter <- function(indexImage, sclImage, bands, invert = FALSE){
  if(invert){
    indexImage[!(sclImage %in% bands)] <- NA
  } else {
    indexImage[!(sclImage %in% bands)] <- NA
  }
  indexImage
}
