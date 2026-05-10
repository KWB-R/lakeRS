#' Classify yearly index values into discrete classes
#'
#' Assigns each yearly index value to one of `nClass` classes. Break points are
#' computed separately for each year using the observed minimum and maximum as
#' outer bounds and equally spaced inner breaks between lower and upper quantiles.
#'
#' @param yearly_spread Data frame with one or more columns whose names start
#'   with `year_`.
#' @param nClass Integer. Number of classes to create per year. Default is `10`.
#' @param proportionExtreme Numeric between 0 and 1. Initial proportion used for
#'   the lower and upper quantiles that define the inner range. Default is `0.05`.
#'
#' @return A list with `assessment`, `breaks`, `nClass`, and
#'   `proportionExtreme`. `assessment` is the input data frame with additional
#'   `<year>_class` columns. `breaks` contains the class boundaries for each year.
#'
#' @details If the requested extreme proportion would create class sizes smaller
#'   than one class share, it is reduced to `1 / nClass`. #' Breaks are constructed using actual data min/max for outer bounds and creates 
#'   `nClass` - 2 \bold{equally spaced} classes between the proportionExtreme quantiles 
#'   (1 - `proportionExtreme` and  0 + `proportionExtreme`).
#'
#' @importFrom stats quantile
#' @export
#' 
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
      "classes" =  cut(
        x = v, 
        breaks = breaks, 
        labels = 1:nClass, 
        include.lowest = TRUE)
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



