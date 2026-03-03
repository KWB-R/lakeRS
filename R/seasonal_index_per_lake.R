#' Aggregation of all pixel NDTrIs into one lake NDTrI
#' 
#' @param seasonIndex A list of NDTrI per pixel of one or more years, created by
#' [seasonal_index_per_pixel()]
#' @param lakeName Character string specifying the name of the lake. 
#' Defaults to \code{NULL}, which assigns \code{NA}.
#' @param lakeID Character string specifying a unique identifier for the lake. 
#'   Defaults to \code{NULL}, which triggers automatic generation in the format 
#'   \code{"<mean_x>_<mean_y>"} coordinate.
#'
#' @importFrom stats density median sd
#' @export
#' 
seasonal_index_per_lake <- function(seasonIndex, lakeName = NULL, lakeID = NULL)
{
  mat <- seasonIndex$RSindex
  validPixel <- sum(!is.na(mat))
  
  output <- if(validPixel > 1){
    d <- stats::density(
      mat, 
      bw = "SJ", 
      adjust = 2, 
      na.rm = TRUE)
    peak_i <- which(diff(sign(diff(d$y))) == -2) 
    modus <- d$x[peak_i[d$y[peak_i] > 1.5 * mean(d$y)]]
    
    best_guess <- 
      if(length(modus) > 0L){
        x_modus <- which(d$x %in% modus)
        modus[order(d$y[x_modus], decreasing = TRUE)[1]]
      } else {
        NA
      }
    
    list("IndexModusBest" = best_guess,
         "IndexMedian" = median(mat, na.rm = TRUE),
         "IndexSD" = sd(mat, na.rm = TRUE),
         "IndexModusPot" = modus,
         "nValidePixel" = validPixel,
         "QualityThreshold" = seasonIndex$QualityThreshold,
         "IndexDensity" = d,
         "IndexPixel" = mat
    )
  } else if(validPixel == 1L){
    singleValue <- median(mat, na.rm = TRUE)
    list("IndexModusBest" = singleValue,
         "IndexMedian" = singleValue,
         "IndexSD" = NA,
         "IndexModusPot" = singleValue,
         "nValidePixel" = 1,
         "QualityThreshold" = seasonIndex$QualityThreshold,
         "IndexDensity" = NA,
         "IndexPixel" = mat
    )
  } else {
    list("IndexModusBest" = NA,
         "IndexMedian" = NA,
         "IndexSD" = NA,
         "IndexModusPot" = NA,
         "nValidePixel" = 0,
         "QualityThreshold" = seasonIndex$QualityThreshold,
         "IndexDensity" = NA,
         "IndexPixel" = NA
    )
  }
  
  output$x <- seasonIndex$x
  output$y <- seasonIndex$y
  output$crs <- seasonIndex$crs
  output$year <- seasonIndex$year
  
  add_lakeID(
    lakeIndex = output,
    lakeName = lakeName,
    lakeID = lakeID
  )
}

#' Add Lake Identifier and Name to Lake Index List
#'
#' This function appends lake identification information (a unique ID and/or name) 
#' to a list object created by [seasonal_index_per_lake()].
#'
#' @param lakeIndex A list produced by [seasonal_index_per_lake()], containing elements such as 
#'   \code{x}, \code{y}, and lake-specific index information.
#' @param lakeName Character string specifying the name of the lake. 
#' Defaults to \code{NULL}, which assigns \code{NA}.
#' @param lakeID Character string specifying a unique identifier for the lake. 
#'   Defaults to \code{NULL}, which triggers automatic generation in the format 
#'   \code{"<mean_x>_<mean_y>"} coordinate.
#'
#' @return The same \code{lakeIndex} list with two additional elements:
#'   \describe{
#'     \item{\code{Name}}{The assigned lake name (character or \code{NA}).}
#'     \item{\code{ID}}{The lake identifier (character).}
#'   }
#'
#' @export
#' 
add_lakeID <- function(lakeIndex, lakeName = NULL, lakeID = NULL){
  if(is.null(lakeID)){
    lakeID <- paste0(mean(lakeIndex$x), "_", mean(lakeIndex$y))
  }
  if(is.null(lakeName)){
    lakeName <- NA
  }
  lakeIndex$Name <- lakeName
  lakeIndex$ID <- lakeID
  lakeIndex
}

