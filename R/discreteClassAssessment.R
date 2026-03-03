#' Discrete Class Assessment for Yearly Index Values
#'
#' This function performs discrete classification of index values from a 
#' into `nClass` classes per year, using actual min/max values as outer bounds 
#' and dynamically adjusting the extreme value proportion.
#'
#' @param yearly_spread Dataframe with one or more columns 
#'   starting with \code{"year_"} containing calculated index values.
#' @param nClass Integer, number of classes per year (default: 10).
#' @param proportionExtreme Numeric, initial proportion of extreme values to 
#'   exclude from inner quantile breaks (default: 0.05). Automatically adjusted 
#'   if class sizes would be too small.
#'
#' @return A list containing:
#'   \describe{
#'     \item{\code{assessment}}{Matrix with class assignments}
#'     \item{\code{breaks}}{Matrix with class boundaries 
#'       (rows=boundaries, columns=years).}
#'     \item{\code{nClass}}{Number of classes used.}
#'     \item{\code{proportionExtreme}}{Final proportion of extreme values used.}
#'   }
#'
#' @details 
#' The function automatically identifies year columns (pattern: \code{"^year_"}). 
#' Breaks are constructed using actual data min/max for outer bounds and creates 
#' `nClass` - 2 \bold{equally spaced} classes between the proportionExtreme quantiles 
#' (1 - `proportionExtreme` and  0 + `proportionExtreme`).
#'
#' @examples
#' \dontrun{
#' # Assuming yearly_spread has columns: ID, year_2020, year_2021, etc.
#' result <- discreteClassAssessment(yearly_spread, nClass = 8, proportionExtreme = 0.03)
#' head(result$assessment)
#' result$breaks
#' }
#'
#' @importFrom stats quantile
#' @export
discreteClassAssessment <- function(
    yearly_spread, nClass = 10, proportionExtreme = 0.05
){
  
  nRows <- nrow(yearly_spread)
  rowsPerClass <- nRows / nClass
  singleYearColumns <- grep(pattern = "^year_",  x = colnames(yearly_spread))
  
  if(rowsPerClass < proportionExtreme * nRows){
    proportionExtreme <- rowsPerClass / nRows
  }
  
  yearlyClassification <- lapply(singleYearColumns, function(i){
    v <- yearly_spread[,i]
    breaks <- c(
      min(v, na.rm = TRUE),
      seq(from = quantile(v, probs = proportionExtreme, na.rm = TRUE), 
          to = quantile(v, probs = 1 - proportionExtreme, na.rm = TRUE), 
          length.out = nClass - 1), 
      max(v, na.rm = TRUE))
    list(
      "breaks" = breaks,
      "classes" =  as.character(cut(
        x = v, 
        breaks = breaks, 
        labels = 1:nClass, 
        include.lowest = TRUE)
      )
    )
  })
  
  yearlyClasses <- do.call(
    cbind, 
    lapply(yearlyClassification, function(x){x$classes})
  )
  colnames(yearlyClasses) <- 
    paste0(colnames(yearly_spread)[singleYearColumns], "_class")
  yearlyBreaks <- do.call(
    cbind, 
    lapply(yearlyClassification, function(x){x$breaks})
  )
  colnames(yearlyBreaks) <- colnames(yearly_spread)[singleYearColumns]
  rownames(yearlyBreaks) <- 1:(nClass + 1)
  
  list("assessment" = 
         do.call(cbind, list(
           yearly_spread, 
           yearlyClasses)
         ),
       "breaks" = yearlyBreaks,
       "nClass" = nClass,
       "proportionExtreme" = proportionExtreme)
}



