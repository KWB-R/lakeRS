#' A band or index sorted by the day of the year
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
#' 10 days before and ten days after the actual day are used for averaging)
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
#' @export
#' 
#' 
dynamic_per_pixel <- function(
    ncImage, t_date, years, water_scenes_only = TRUE, days_around_ma = 20, 
    maxPixels = 1000, threshold = NULL, lakeInfo = c("", ""), maxDataPoints = 5000000
){
  output <- list()
  output[["lakeInfo"]] <- lakeInfo
  
  imageDOY <- as.numeric(format(t_date, "%j"))
  imageYear <- as.numeric(format(t_date, "%Y"))
  
  yearFilter <- imageYear %in% years
  
  indexImages <- ncImage$RSindex[yearFilter]
  imageDOY <- imageDOY[yearFilter]
  d <- dim(indexImages[[1]])
  
  sclImage <- ncImage$SCL[yearFilter]
  
  cat("Image data is filtered pixel by pixel ... \n")
  
  if(water_scenes_only){
    indexImages <- lapply(seq_along(indexImages), function(i){
      indexImages[[i]][sclImage[[i]] != 6] <- NA
      indexImages[[i]]
    })
  } else {
    indexImages <- lapply(seq_along(indexImages), function(i){
      indexImages[[i]][sclImage[[i]] %in% c(8:11)] <- NA
      indexImages[[i]]
    })     
  }
  
  t_doy <- imageDOY[order(imageDOY)]
  indexImages_doy <- indexImages[order(imageDOY)]
  
  cat("Data is reshaped from spatial image data to timeseries per pixel ... \n")
  ts <- matrix(
    data = unlist(indexImages_doy), 
    nrow = d[1] * d[2], 
    ncol = length(indexImages_doy)
  )
  
  # # test the order matrix to vector data
  # identical(
  #   indexImages_doy[[1]], 
  #   matrix(data = ts[,1], nrow = 65, ncol = 57))
  
  if(is.null(threshold)){
    threshold <- 365 / days_around_ma * 10 
  }
  
  valid_values <- apply(ts, 1 , function(p_ts){sum(!is.na(p_ts))})
  pixel_selection <- valid_values > threshold
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
      nm, " small matrices ... \n")
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
  
  cat(paste0("Processing ", nm, " matrices ... \n"))
  df_out <- lapply(ts_start, function(ts){
    cat(" | matrix done")
    lake_output <- lapply(1:nrow(ts), function(p_i){
      moving_average(
        ts_pixel = ts[p_i,], 
        t_doy = t_doy,
        days_around_ma = days_around_ma
      )
    })
    df_out <- do.call(cbind, lake_output)
    df_out[,-grep(pattern = "doy", colnames(df_out))[-1]]
  })
  cat(" | all matrixes done \n")

  if(length(df_out) > 1L){
    df_out <- do.call(cbind, df_out)
    df_out <- df_out[,-grep(pattern = "doy", colnames(df_out))[-1]]
  } else {
    df_out <- df_out[[1]]
  }
  
  colnames(df_out)[2:ncol(df_out)] <- paste0("p_", 1:(ncol(df_out)-1))
  
  i_col <- ceiling(pixel_selection_i/ d[1])
  i_row <- pixel_selection_i - d[1] * (i_col - 1)
  
  list("moving_averages" = df_out,
       "raster_location" = data.frame(
         "pixel" = colnames(df_out)[2:ncol(df_out)],
         "i_col" = i_col,
         "i_row" = i_row,
         "valid_values" = valid_values[pixel_selection_i])
  )
}

#' Moving average for a sorted pixel vector with according day of the year time 
#' vector
#' 
#' @param ts_pixel Data for one pixel of multiple images (ts)
#' @param t_doy Numeric vector of the day of the year according to ts_pixel 
#' (same length and order)
#' @param days_around_ma The days around the moving average day (i.e. 10  means
#' 10 days before and ten days after the actual day are used for averaging)
#' 
#' 
moving_average <- function(ts_pixel, t_doy, days_around_ma){
  df_out <- data.frame("doy" = 1:365)
  
  df_out$ma <- sapply(df_out$doy, function(doy){
    d_range <- doy + c(-days_around_ma, days_around_ma)
    d_range[d_range < 1] <- d_range[d_range < 1] + 365
    d_range[d_range > 365] <- d_range[d_range > 365] - 365
    
    days_for_ma <- if(d_range[2] > d_range[1]){
      t_doy > d_range[1] & t_doy < d_range[2]
    } else {
      t_doy > d_range[1] | t_doy < d_range[2]
    }
    mean(ts_pixel[days_for_ma], na.rm = TRUE) 
  })
  df_out
}
