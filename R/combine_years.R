#' Combine annual lake index results into a harmonized pixel-wise data structure
#'
#' This function merges multiple annual lake index maps (each produced by
#' [seasonal_index_per_lake()]) into a unified per-pixel dataset. It ensures that
#' all years are represented on a common spatial grid. If differences in CRS
#' or grid alignment between years exist, the rasters are resampled to match
#' the most recent year's configuration.
#'
#' @param lakeIndexList A list of yearly lake index lists created by
#'   [seasonal_index_per_lake()]. Each element must contain (at minimum)
#'   `IndexPixel`, `x`, `y`, `crs`, and `year` components.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{`yearsIndex`}{A `data.frame` where each row corresponds to a pixel position
#'   (`x`, `y`, `i_row`, `i_col`) and each `yYYYY` column stores the pixel values
#'   of that year.}
#'   \item{`crs`}{A character string giving the CRS of the final combined grid.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates input to ensure \code{lakeIndexList} is a list of lists.
#'   \item Extracts available years and their corresponding CRS values.
#'   \item Selects the most recent year to define the target grid.
#'   \item For each year, checks grid compatibility; if needed, resamples to the
#'         target resolution and CRS using bilinear interpolation.
#'   \item Merges all years’ pixel tables into a single data frame by coordinates. 
#' }
#'
#' @seealso [seasonal_index_per_lake()], [terra::resample()], [terra::rast()]
#'
#' @export
#' 
combine_years_pixelData <- function(lakeIndexList){
  if(!is.list(lakeIndexList[[1]])){
    stop("lakeIndexList needs to be a list of lists created by 'seasonal_index_per_lake()'")
  }
  
  years <- sapply(lakeIndexList, function(x){x$year})
  if(any(duplicated(years))){
    stop("At least one year appears more than once on the list.")
  }
  
  crs_of_years <- sapply(lakeIndexList, function(x){x$crs})
  if(length(unique(crs_of_years)) > 1L){
    warning("Different coordinate reference systems were used. ",
            "All years will be resampled to the CRS used most recently.")
  }
  most_recent <- order(years, decreasing = TRUE)[1]
  mr <- lakeIndexList[[most_recent]]
  
  x_res <- abs(unique(diff(mr$x)))
  y_res <- abs(unique(diff(mr$y)))
  
  final_raster <- terra::rast(
    x = mr$IndexPixel,
    extent = c(min(mr$x) -  x_res/2, 
               max(mr$x) +  x_res/2, 
               min(mr$y) -  y_res/2, 
               max(mr$y) +  y_res/2),
    crs  = mr$crs
  ) 
  
  list_of_years <- lapply(lakeIndexList, function(x){
    x_res <- abs(unique(diff(x$x)))
    y_res <- abs(unique(diff(x$y)))
    
    if(!all(c(x$crs == mr$crs, x$x == mr$x, x$y == mr$y))){
      r_in <- terra::rast(
        x = x$IndexPixel,
        extent = c(
          min(x$x) -  x_res/2, 
          max(x$x) +  x_res/2, 
          min(x$y) -  y_res/2, 
          max(x$y) +  y_res/2),
        crs = x$crs
      ) 
      r_in_resampled <- terra::resample(r_in, final_raster, method = "bilinear")
      df_out <- terra::xyFromCell(
        object = r_in_resampled, 
        cell = 1:terra::ncell(r_in_resampled)
      )
      df_out <- df_out[order(df_out[,"x"]),]
      df_out <- data.frame(df_out[,c("y", "x")])
      v <- terra::values(r_in_resampled)
    } else {
      df_out <- expand.grid("y" = x$y, "x" = x$x)
      v <- c(x$IndexPixel)
    }
    df_out$i_row <- as.numeric(factor(df_out$y, levels = x$y))
    df_out$i_col <- as.numeric(factor(df_out$x, levels = x$x))
    df_out[[paste0("year_", x$year)]] <- v
    df_out
  })
  
  yearly_spread <- Reduce(
    f = function(x,y){
      merge(
        x = x, 
        y = y, 
        by = c("x", "y", "i_row", "i_col")
      ) 
    }, 
    x = list_of_years)
  attr(yearly_spread, "out.attrs") <- NULL
  
  list("indexTable" = yearly_spread,
       "crs" = mr$crs)
}

#' Combine lake-level index values across years
#'
#' Converts a list of annual lake index objects into a wide table with one row
#' per lake and one index column per year.
#'
#' @param lakeIndexList A list of lists produced by [seasonal_index_per_lake()].
#'   Each element must contain `year`, `Name`, `ID`, and the requested index
#'   statistic.
#' @param aggregationType Character scalar. Use `"modus"` to extract
#'   `IndexModusBest` or `"median"` to extract `IndexMedian`.
#'
#' @return A `data.frame` with lake identifiers (`name`, `id`) and one wide
#'   `year_<year>` column per available year.
#'
#' @details The function warns if lake names or IDs are not unique in the output.
#'
#' @importFrom tidyr spread
#' @export
#' 
combine_years_lakeData<- function(lakeIndexList, aggregationType = "modus"){
  if(!is.list(lakeIndexList[[1]])){
    stop("lakeIndexList needs to be a list of lists created by 'seasonal_index_per_lake()'")
  }
  
  dl <- lapply(lakeIndexList, function(x){
    v_out <- c("year" = x$year, "name" = x$Name, "id" = x$ID)
    if(aggregationType == "modus"){
      v_out <- c(v_out, "index" = x$IndexModusBest)
    } else if(aggregationType == "median"){
      v_out <- c(v_out, "index" = x$IndexMedian)
    }
    v_out
  })
  
  df <- data.frame(do.call(rbind, dl))
  df_out <- tidyr::spread(
    data = df, 
    key = "year", 
    value = "index", 
    fill = NA, 
    sep = "_"
  )
  if(any(duplicated(df_out$name)) | any(duplicated(df_out$id))){
   warning("At least one not unique combination of ID and lake name") 
  }
  df_out
}

#' Combine whole-lake dynamics across years
#'
#' Combines multiple annual outputs from [dynamic_per_pixel()] into summary
#' curves across years. For each day of year, the function averages the annual
#' median lake dynamics and calculates the standard deviation across those annual
#' medians.
#'
#' @param indexDynamicList A named or unnamed list of annual dynamic objects
#'   created by [dynamic_per_pixel()]. Each element must contain `lakeDynamic`
#'   with column `q_0.5` and `days_around_ma`.
#' @param years Optional numeric vector of years to include. If `NULL`, all list
#'   elements are used. If the list is named, matching is performed on the names.
#'
#' @return A list with `mean_of_median` and `sd_over_median`, each a numeric
#'   vector with one value per day of year.
#'
#' @details All selected dynamics must have been computed with the same
#'   `days_around_ma`; otherwise the function stops because the smoothing windows
#'   are not comparable.
#'
#' @export
#' 
combine_years_dynamic <- function(indexDynamicList, years = NULL){
  i <- seq_along(indexDynamicList)
  if(!is.null(years)){
    i <- grep(
      pattern = paste0(years, collapse = "|"), 
      x = names(indexDynamicList)
    ) 
  }
  idl <- indexDynamicList[i] 
  dama <- unique(sapply(idl, function(x){x$days_around_ma}))
  if(length(dama) > 1L){
    stop("The yearly dynamics were calculated with different time intervals for",
         " for the moving average and should not be combined")
  }
  
  ld_median <- lapply(indexDynamicList[i], function(x){
    x$lakeDynamic$q_0.5
  })
  df <- do.call(cbind, ld_median)
  list("mean_of_median" = apply(df, 1, mean, na.rm = TRUE),
       "sd_over_median" = apply(df, 1, sd, na.rm = TRUE))
}
