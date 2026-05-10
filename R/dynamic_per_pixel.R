#' Calculate day-of-year moving-average dynamics per pixel
#'
#' Converts image-wise remote-sensing index matrices into pixel-wise time series
#' and calculates smoothed 365-day dynamics. The output includes lake-level
#' summary statistics and, optionally, the individual pixel dynamics.
#'
#' @param imageIndex A list created by [ndi_per_image()] containing `RSindex`,
#'   `t_date`, `t`, `x`, `y`, and `crs`.
#' @param nc A netCDF list returned by [open_netcdf()]. The SCL band is loaded
#'   from this object for scene filtering.
#' @param water_scenes_only Logical. If `TRUE`, only pixels with SCL class 6
#'   (water) are retained before averaging. If `FALSE`, cloudy pixels (SCL 
#'   8,9,10,11) are removed before averaging. 
#' @param days_around_ma Integer or `NULL`. Half-window size, in days, around
#'   each day of year. (i.e. 10  means 10 days before and ten days after the 
#'   actual day are used for averaging). If `NULL`, it is derived from the median number of valid
#'   observations per pixel.
#' @param maxPixels Numeric. Maximum number of pixels for which individual
#'   dynamics are calculated. Values `<= 1` are interpreted as a proportion of
#'   valid pixels; `Inf` uses all valid pixels. Ignored when `pixelFilter` is set.
#' @param pixelFilter Optional integer vector of pixel positions in the original
#'   matrix order. If supplied, exactly these pixels are processed.
#' @param pixelQualityThreshold Optional numeric scalar between 0 and 1. 
#'   Minimum water-scene proportion required for a pixel. If "byMedian", the median 
#'   number of valid observations among non-empty pixels is used to select 
#'   pixels.
#' @param maxDataPoints Numeric. Maximum number of matrix cells processed in one
#'   block. Larger datasets are split to reduce memory pressure.
#' @param returnSinglePixels Logical. If `TRUE`, returns `pixelDynamics` and the
#'   selected pixel coordinates. If `FALSE`, only lake-level summaries are
#'   returned.
#'
#' @return A list with `lakeDynamic`, `crs`, `year`, `days_around_ma`, and
#'   `dataAvailabilityThreshold`. If `returnSinglePixels = TRUE`, the list also
#'   contains `x`, `y`, and `pixelDynamics`.
#'
#' @details For each day of year, all images within `days_around_ma` days before
#'   and after the target day are averaged. A moving average is only calculated
#'   if more than one image falls into the window. Long periods without image
#'   availability are set to `NA` to avoid excessive temporal extrapolation.
#'
#' @export
#' 
dynamic_per_pixel <- function(
    imageIndex, nc, water_scenes_only = TRUE, days_around_ma = 20, 
    maxPixels = 1000, pixelFilter = NULL, pixelQualityThreshold = 0.8,
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
    indexList[[i]] <- 
      if(water_scenes_only){
        scl_mask(
          indexImage = indexList[[i]], 
          sclImage = sclList[[i]], 
          sclCategories = 6, 
          invert = TRUE)
      } else {
        scl_mask(
          indexImage = indexList[[i]], 
          sclImage = sclList[[i]], 
          sclCategories = c(8:11))    
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
    filteredIndexList <- lapply(indexList, function(x){x[pixelFilter]})
    ts_select <- matrix(
      data = unlist(filteredIndexList), 
      nrow = np, 
      ncol = length(filteredIndexList)
    )
  } else {
    ts <- sapply(indexList, function(x){
      unlist(x)
    })
    valid_values <- apply(ts, 1 , function(p_ts){sum(!is.na(p_ts))})
    med_values <- median(valid_values[valid_values > 0])
    if(pixelQualityThreshold == "byMedian"){
      nDataPointsThreshold <- med_values
      pixel_selection <- valid_values >= nDataPointsThreshold
      pixelQualityThreshold <- nDataPointsThreshold / length(ts)
    } else {
      waterLayer <- lakeRS::waterscene_proportion(scl_image = sclList)
      pixel_selection <- waterLayer$water >= pixelQualityThreshold
      pixel_selection[is.na(pixel_selection)] <- FALSE
    }
    if(is.null(days_around_ma)){
      days_around_ma <- ceiling(365 / med_values * 3)
    }
    
    if(maxPixels <= 1){
      maxPixels <- round(sum(pixel_selection) * maxPixels)
    }
    pixel_selection_i <- 
      if(maxPixels < sum(pixel_selection)){
        order(valid_values, decreasing = TRUE)[1:maxPixels]
      } else {
        which(pixel_selection)
      }
   
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
    ts_select <- ts[pixel_selection_i,]
    rm(ts)
  }
  
  equal_size_nRow <- ceiling(maxDataPoints / dim(ts_select)[2])
  nm <- ceiling(dim(ts_select)[1]/equal_size_nRow)
  if(prod(dim(ts_select)) > maxDataPoints){
    nm <- nm + 1
    cat(paste0(paste0(
      "Due to large matrix size, data is split into ", 
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
    "Calculate moving averages of ", length(pixel_selection_i), " pixel timesseries ... \n")
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
    "dataAvailabilityThreshold" = pixelQualityThreshold
  )
  if(returnSinglePixels){
    pixelDynamics <- as.list(as.data.frame(df_out))
    output[["x"]] <- imageIndex$x[i_col]
    output[["y"]] <- imageIndex$y[i_row]
    output[["pixelDynamics"]] <- pixelDynamics
  } 
  output
}


#' Identify images used for each moving-average day
#'
#' Builds a 365-element list of logical vectors indicating which image dates fall
#' within the moving-average window around each day of year.
#'
#' @param t_doy Numeric vector of image days of year, in the same order as the
#'   image time series.
#' @param days_around_ma Integer. Half-window size in days.
#'
#' @return A list of length 365. Each element is a logical vector with the same
#'   length as `t_doy`.
#'
#' @details Windows wrap around the end of the year. The implementation uses
#'   strict inequalities, so images exactly on the lower or upper window boundary
#'   are not included.
#'
#' @keywords internal
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


