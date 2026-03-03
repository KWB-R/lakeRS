#' The moving average dynamic of an RS index
#' 
#' The dynamic can include many years, all sorted by the day of the year.
#' The aggregation per pixel is a moving average.
#' 
#' @param imageIndex the data list created by [ndi_per_image()]
#' @param nc The netcdf list returned by [open_netcdf()]
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
#' @param pixelFilter The ID of pixels to be used (The ID of a pixel is its 
#' its number in the the original matrix of the netcdf).
#' @param maxDataPoints The number of datapoints to be part of one matrix
#' for moving average calculation. The default (5E+06) corresponds to 5000 pixels
#' in 1000 images. 
#' @param returnSinglePixels If TRUE (default) moving average dynamic of each
#' pixel (up to the number of maxPixels) is returned. If False only median and
#' some quantiles are returned.
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
    imageIndex, nc, water_scenes_only = TRUE, days_around_ma = 20, 
    maxPixels = 1000, pixelFilter = NULL, threshold = NULL,
    maxDataPoints = 5000000, returnSinglePixels = TRUE
){
  year <- unique(as.numeric(format(imageIndex$t_date, "%Y")))
  months <- unique(as.numeric(format(imageIndex$t_date, "%m")))
  sclList <- load_BandLayer(
    nc = nc, 
    band = "SCL", 
    monthFilter = months, 
    yearFilter = year
  )$band
  indexList <- imageIndex$RSindex
  if(length(sclList) != length(indexList)){
    stop("SCL and Index list length do not match.")
  }
  
  cat("Image data is filtered for SCL categories pixel by pixel ... \n")
  for(i in seq_along(indexList)){
    indexList[[i]] <- if(water_scenes_only){
      scl_filter(
        indexImage = indexList[[i]], 
        sclImage = sclList[[i]], 
        bands = 6, 
        invert = TRUE)
    } else {
      scl_filter(
        indexImage = indexList[[i]], 
        sclImage = sclList[[i]], 
        bands = c(8:11))    
    }
  }
  
  imageDOY <- as.numeric(format(imageIndex$t_date, "%j"))
  t_doy <- imageDOY[order(imageDOY)]
  indexList <- indexList[order(imageDOY)]
  d <- dim(indexList[[1]])
  
  cat("Data is reshaped from spatial image data to timeseries per pixel ... \n")
  if(!is.null(pixelFilter)){
    pixel_selection_i <- pixelFilter
    nAvailable <- np <- length(pixelFilter)
    maxPixels <- Inf
    filteredIndexList <- lapply(indexList, function(x){x[pixelFilter]})
    ts_select <- matrix(
      data = unlist(filteredIndexList), 
      nrow = np, 
      ncol = length(filteredIndexList)
    )
  } else {
    ts <- matrix(
      data = unlist(indexList), 
      nrow = d[1] * d[2], 
      ncol = length(indexList)
    )
    valid_values <- apply(ts, 1 , function(p_ts){sum(!is.na(p_ts))})
    med_values <- median(valid_values[valid_values > 0])
    if(is.null(threshold)){
      threshold <- med_values
    }
    if(is.null(days_around_ma)){
      days_around_ma <- ceiling(365 / med_values * 3)
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
    ts_select <- ts[pixel_selection_i,]
    rm(ts)
  }
  
  equal_size_nRow <- ceiling(maxDataPoints / dim(ts_select)[2])
  nm <- ceiling(dim(ts_select)[1]/equal_size_nRow)
  if(prod(dim(ts_select)) > maxDataPoints){
    nm <- nm + 1
    cat(paste0(paste0(
      "Due to large matrix size, data is splitted into ", 
      nm, " small matrices ... \n")
    ))
    
    ts_parts <- list()
    i <- 1
    while(nrow(ts_select) > equal_size_nRow){
      ts_parts[[i]] <- ts_select[1:equal_size_nRow,]
      ts_select <- ts_select[-(1:equal_size_nRow),]
      i <- i + 1
    }
    ts_parts[[i]] <- ts_select[1:nrow(ts_select),]
    ts_select <- ts_parts
    rm(ts_parts)
    gc()
  }  else {
    ts_select <- list(ts_select)
  }
  
  cat(paste0(
    "Calculate moving averages of ", nAvailable, " pixel timesseries ... \n")
  )
  ipa <- images_per_ma(
    t_doy = t_doy,
    days_around_ma = days_around_ma
  )
  
  cat(paste0("Processing ", nm, " matrices ... \n"))
  df_out <- lapply(ts_select, function(x){
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
  
  colnames(df_out) <- paste0("p_", pixel_selection_i)
  lakeDynamic <- data.frame("doy" = 1:365, 
                            "mean" = rowMeans(df_out, na.rm = TRUE))
  quantProbs <-  c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
  value_stats <- t(
    apply(
      X = df_out, 
      MARGIN = 1, 
      FUN = quantile, 
      probs = quantProbs, 
      names = FALSE,
      na.rm = TRUE
    )
  ) 
  colnames(value_stats) <- paste0("q_", quantProbs)
  lakeDynamic <- cbind(lakeDynamic, value_stats)
  
  i_col <- ceiling(pixel_selection_i/ d[1])
  i_row <- pixel_selection_i - d[1] * (i_col - 1)
  
  output <- list(
    "lakeDynamic" = lakeDynamic,
    "crs" = imageIndex$crs,
    "year" = year,
    "days_around_ma" = days_around_ma,
    "dataAvailabilityThreshold" = threshold
  )
  if(returnSinglePixels){
    pixelDynamics <- as.list(as.data.frame(df_out))
    output[["x"]] <- imageIndex$x[i_col]
    output[["y"]] <- imageIndex$y[i_row]
    output[["pixelDynamics"]] <- pixelDynamics
  } 
  output
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


