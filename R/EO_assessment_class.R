#' The assessment of eutriphication based on ranking in classes
#' 
#' @param yearly_spread A dataframe of lakeName, lakeID and the yearly NDTrI 
#' values as created by [ndtri_spread()]
#' @param nClass The number of class used (3 classes minimum). 
#' 
#' @details
#' The first and last class are always defined as the least and most 
#' eutrophicated 5% of all lakes, respectively. The remaining breaks are equal 
#' in range, i.e. the classes can contain different amounts of lakes.
#' 
#' @return
#' A list of 3: 
#' "assessment": the yearly_spread dataframe extended by class
#' columns. 
#' "breaks": The breaks used for the classification in each year.
#' "nClass": The total number of classes 
#' "proportionExtreme": The proportion of lakes in the first and last class
#' 
#' @export
#' 
EO_assessment_class <- function(
    yearly_spread, nClass = 10
){
  
  # Class results
  Cl <- determine_classes(yearly_spread = yearly_spread, nClass = nClass)
  
  list(
    "assessment" = 
      do.call(cbind, list(
        yearly_spread, 
        Cl$classes)
      ),
    "nClass" = Cl$nClass,
    "proportionExtreme" = Cl$proportionExtreme
  )
}

#' The class of NDTrI is determined compared to other lakes per year
#' 
#' @param yearly_spread A dataframe of lakeName, lakeID and the yearly NDTrI 
#' values as created by [ndtri_spread()]
#' @param nClass The number of class used (3 classes minimum). 
#' 
#' @details
#' The first and last class are by default defined as the least and most 
#' eutrophicated 5% of all lakes, respectively. However, the classes should 
#' always contain less lakes as the average number of lakes per class. The 
#' quantile limit is adjusted if the number of classes is extremely high compared
#' to the number of lakes. The remaining classes between first and last one, 
#' are equal in range to avoid high sensitivity regions in the ranking, 
#' i.e. the classes can contain different amounts of lakes.
#' 
#' @return 
#' A list of 4:
#' 
#' "classes": A matrix of classes as characters, where number of rows equal the number of 
#' lakes (in the same order as in the yearly_spread input table) and number of 
#' columns correspond to the number of years assessed
#' "breaks": The breaks of the classes.
#' "nClass": the total number of classes
#' "upperLowerLimit": The proportion of lakes in the first and last class
#' 
#' @importFrom stats quantile
#' 
determine_classes <- function(yearly_spread, nClass = 10){
  
  upperLowerLimit <- 0.05
  nLakes <- nrow(yearly_spread)
  lakesPerClass <- nLakes / nClass
  
  if(lakesPerClass < upperLowerLimit * nLakes){
    upperLowerLimit <- lakesPerClass / nLakes
  }
  
  lastYearColumn <- ncol(yearly_spread)
  singleYearColumns <- grep(pattern = "^year_",  x = colnames(yearly_spread))
  
  yearlyClassification <- lapply(singleYearColumns, function(i){
    year_i <- yearly_spread[,i]
    breaks <- c(
      -1,
      seq(from = quantile(year_i, probs = upperLowerLimit), 
          to = quantile(year_i, probs = 1 - upperLowerLimit), 
          length.out = nClass - 1), 
      1)
    list(
      "breaks" = breaks,
      "classes" =  as.character(cut(
        x = year_i, 
        breaks = breaks, 
        labels = 1:nClass, 
        include.lowest = TRUE
      )
    ))
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
  
  
 
  
  list("classes" = yearlyClasses,
       "breaks" = yearlyBreaks,
       "nClass" = nClass,
       "proportionExtreme" = upperLowerLimit)
}

