#' The assessment of eutriphication based on NDTrI Values
#' 
#' @param yearly_spread A dataframe of lakeName, lakeID and the yearly NDTrI 
#' values as created by [ndtri_spread()]
#' @param statusYears The number of years used for the moving average. By default 
#' this is 3 years.
#' @param shortTermYears The number of years used for the short-term trend.
#' @param longTermYears The number of years used for the short-term trend.
#' 
#' @details
#' The status of NDTrI is determined as moving average over n years.
#' 
#' In order to rule out systematic errors based on weather conditions in a year,
#' different hardware or algorithms, the difference between the overall median value
#' of all lakes and years and the yearly median value of all lakes is added to the 
#' NDTrI of a lake before the trends are calculated.
#' Subsequantially, the trend is calculated as linear regression between previous
#' years and adjusted NDTrI values.
#' For a reliable trend assessment the number of lakes needs to be large enough 
#' (n>10).
#' 
#' @return
#' A list of 4: 
#' "assessment": the yearly_spread dataframe  extended by status and trend 
#' columns. For each time period, the trend column is the slope of the 
#' regression line while error is the standard error of the slope.
#' "periods": number of years used for status, short-term and long-term trend
#' "overallMedian": median value of the whole dataset
#' "yearlyMedians": median values of all lakes per year
#' 
#' @export
#' 
EO_assessment_numeric <- function(
    yearly_spread, statusYears = 3, shortTermYears = 3, longTermYears = 10
){
  St <- determine_status(
    yearly_spread = yearly_spread, 
    n_years = statusYears
  )
  Tr <- determine_trend(
    yearly_spread = yearly_spread, 
    shortTerm = shortTermYears, 
    longTerm = longTermYears
  )
  
  list(
    "assessment" = 
      do.call(cbind, list(
        yearly_spread, 
        St$status, 
        Tr$trends)
      ),
    "periods" = c(unlist(St["nYears"]), Tr$periods),
    "overallMedian" = Tr$overallMedian,
    "yearlyMedians" = Tr$yearlyMedians
  )
    
}

#' The status of NDTrI is determined as moving average over n years
#' 
#' @param yearly_spread A dataframe of lakeName, lakeID and the yearly NDTrI 
#' values as created by [ndtri_spread()]
#' @param n_years The number of years used for the moving average. By defaul 
#' this is 3 years.
#' 
#' @return 
#' A list of 2:
#' 
#' A matrix, where number of rows equal the number of lakes (in the same order
#' as in the yearly_spread input table) and number of correspond to the number
#' of years assessed
#' 
#' 
determine_status <- function(
    yearly_spread, n_years = 3
){
  first_year_column <- min(grep(pattern = "^year_",colnames(yearly_spread)))
  last_year_column <- ncol(yearly_spread)
  
  df_out <- sapply(first_year_column:(last_year_column - n_years + 1), function(i){
    x <- yearly_spread[,i:(i + n_years - 1)]
    apply(x, 1, mean)
  })
  colnames(df_out) <- 
    paste0(colnames(yearly_spread)[(first_year_column + n_years - 1):last_year_column], "_status")
  list("status" = df_out,
       "nYears" = n_years)
}

#' The Trend of NDTrI is determined within a coelleciton of lakes in short and
#' long-term
#' 
#' 
#' 
#' @param yearly_spread A dataframe of lakeName, lakeID and the yearly NDTrI 
#' values as created by [ndtri_spread()]
#' @param shortTerm The number of years used for the short-term trend.
#' @param longTerm The number of years used for the short-term trend.
#' 
#' @details
#' In order to rule out systematic errors based on weather conditions in a year,
#' different hardware or algorithms, the difference between the overall median value
#' of all lakes and years and the yearly median value of all lakes is added to the 
#' NDTrI of a lake before the trends are calculated.
#' Subsequantially, the trend is calculated as linear regression between previous
#' years and adjusted NDTrI values.
#' For a reliable trend assessment the number of lakes needs to be large enough 
#' (n>10).
#' 
#' @return 
#' A list of 4: 
#' "trends" is a matrix with one row per lake in the as in the 
#' yearlyspread input table and two columns for each trend, short-term and 
#' long-term. The trend is the slope of the regression line, and error is the
#' standard error of the slope.
#' "periods": number of years used for short-term and long-term trend
#' "overallMedian": median value of the whole dataset
#' "yearlyMedians": median values of all lakes per year
#' 
#' @importFrom stats lm
#' 
determine_trend <- function(yearly_spread = yearly_spread, shortTerm = 3, longTerm = 10){
  single_year_columns <- grep(pattern = "^year_", 
                              x = colnames(yearly_spread))
  
  mat <- as.matrix(yearly_spread[,single_year_columns])
  overall_median <- median(mat)
  yearly_medians <- apply(mat, 2, median)
  yearly_shift <- overall_median - yearly_medians
  
  if(shortTerm > ncol(mat)){
    shortTerm <- ncol(mat)
  }
  if(longTerm > ncol(mat)){
    longTerm <- ncol(mat)
  }
  
  shortTermColumns <- (ncol(mat) - shortTerm:1 + 1)
  longTermColumns <- (ncol(mat) - longTerm:1 + 1)
  
  shortTermMat <- mat[,shortTermColumns]
  longTermMat <- mat[,longTermColumns]
  
  
  output <- t(sapply(1:nrow(mat), function(i){
    dfs <- data.frame(
      "Year" = colnames(shortTermMat),
      "NDTrI" = shortTermMat[i,],
      "NDTrI_adjusted" = shortTermMat[i,] + yearly_shift[shortTermColumns],
      "years_passed" = -(shortTerm - 1):0
    )
    dfl <- data.frame(
      "Year" = colnames(longTermMat),
      "NDTrI" = longTermMat[i,],
      "NDTrI_adjusted" = longTermMat[i,] + yearly_shift[longTermColumns],
      "years_passed" = -(longTerm - 1):0
    )
    
    st <- summary(lm(NDTrI_adjusted ~ years_passed, data = dfs))
    lt <- summary(lm(NDTrI_adjusted  ~ years_passed, data = dfl))
    
    c("trend_short" = st$coefficients[2,1],
      "error_short" = st$coefficients[2,2],
      "trend_long" = lt$coefficients[2,1],
      "error_long" = lt$coefficients[2,2])
  }))
  
  list("trends" = output,
       "periods" = c("shortTerm" = shortTerm, "longTerm" = longTerm),
       "overallMedian" = overall_median,
       "yearlyMedians" = yearly_medians)
}

