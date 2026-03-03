#' The assessment of eutriphication based on NDTrI Values
#' 
#' @param yearly_spread A dataframe of lakeName, lakeID and the yearly NDTrI 
#' values as created by [index_spread()]
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
numericAssessment <- function(
    yearly_spread, statusYears = 3, shortTermYears = 3, longTermYears = 10
){
  first_year_column <- min(grep(pattern = "^year_",colnames(yearly_spread)))
  last_year_column <- ncol(yearly_spread)
  available_years <- last_year_column - first_year_column + 1
  if(available_years < statusYears){
    warning("Status calculation not possible, only ", available_years, 
            " years available.")
    yearly_spread$status<- NA
  } else {
    cat("Calculate status of available years ... \n")
    St <- determine_status(
      yearly_spread = yearly_spread, 
      n_years = statusYears
    )
    yearly_spread <- cbind(yearly_spread, St$status)
  }
  if(available_years < shortTermYears){
    warning("Trend calculation not possible, only ", available_years, 
            " years available.")
    sTrend <- lTrend <- NA
    yearly_spread$trend_short <- yearly_spread$trend_long <- NA
  } else {
    cat(paste0(
      "Calculate short-term trend over ", shortTermYears," years ... \n")
    )
    sTrend <- determine_trend(
      yearly_spread = yearly_spread, 
      tyears = shortTermYears, 
      ttype = "short"
    )
    
    yearly_spread <- cbind(yearly_spread, sTrend$trends)
    
  }
  
  if(available_years < longTermYears){
    yearly_spread$trend_long <- NA
    warning("Long-term trend calculation not possible, only ", available_years, 
            " years available.")
  } else {
    cat(paste0(
      "Calculate long-term trend over ", longTermYears," years ... \n")
    )
    lTrend <- determine_trend(
      yearly_spread = yearly_spread, 
      tyears = longTermYears, 
      ttype = "long"
    )
    yearly_spread <- cbind(yearly_spread, lTrend$trends)
    yearly_spread
    
  }
  list(
    "assessment" = yearly_spread,
    "periods" = c("status" = statusYears, 
                  "shortTerm" = shortTermYears, 
                  "longTerm" = longTermYears
    )
  )
}

#' The average status of an index over n years
#' 
#' @param yearly_spread A dataframe of lakes or pixels as row and the yearly 
#' index values as columns as created by [index_spread()]
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

#' Trend of an Index over years
#' 
#' @param yearly_spread A data frame of lakes or pixels as row and the yearly 
#' index values as columns as created by [index_spread()]
#' @param tyears The number of years used for the trend analysis.
#' @param ttype The type of trend (either "short" or "long") as character string
#' 
#' @details
#' In order to rule out systematic errors based on weather conditions in a year,
#' different hardware or algorithms, the difference between the overall median 
#' value of all lakes (or all pixels) and years and the yearly median value of 
#' all lakes(or all pixels) is added to the Index of a lake (or pixel) before 
#' the trends are calculated. Median values are based on lakes or pixels for which data is available in all considered 
#' years.
#' Subsequantly, the trend is calculated as linear regression between previous
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
determine_trend <- function(
    yearly_spread = yearly_spread, tyears = 3, ttype = "short"
){
  if(!(ttype %in% c("short", "long"))){
    stop("ttype needs to be either 'short' or 'long'.")
  }
  single_year_columns <- grep(pattern = "^year_", 
                              x = colnames(yearly_spread))
  
  mat <- as.matrix(yearly_spread[,single_year_columns])
  all_years_available <- apply(mat, 1, function(x)(all(!is.na(x))))
  
  overall_median <- median(mat[all_years_available,])
  yearly_medians <- apply(mat[all_years_available,], 2, median)
  yearly_shift <- overall_median - yearly_medians
  
  if(tyears >= ncol(mat)){
    tyears <- ncol(mat)
  }
  tyearsColumns <- (ncol(mat) - tyears:1 + 1)
  tyearsMat <- mat[,tyearsColumns]
  
  c_out <- t(sapply(1:nrow(tyearsMat), function(i){
    if(any(is.na(tyearsMat[i,]))){
      v <- c(NA,NA)
    } else {
      dfs <- data.frame(
        "Year" = colnames(tyearsMat),
        "NDTrI" = tyearsMat[i,],
        "NDTrI_adjusted" = tyearsMat[i,] + yearly_shift[tyearsColumns],
        "years_passed" = -(tyears - 1):0
      )
      st <- summary(lm(NDTrI_adjusted ~ years_passed, data = dfs))
      v <- c(st$coefficients[2,1], st$coefficients[2,2])
    }
    names(v) <- c(paste0("trend_", ttype), paste0("error_", ttype))
    v
  }))
  list("trends" = c_out,
       "periodYear" = tyears,
       "overallMedian" = overall_median,
       "yearlyMedians" = yearly_medians)
  
}


#' Turn numerical trends into trend significance classes
#'
#' @param assessmentTable The output table created by [numericAssessment()]
#' @param trendType Character string. Either "long" long-term or "short" for 
#' short-term
#' 
#' @details
#' The trend significance is 0 if 0 is within
#' the range of ("trend - error" , "trend + error") (-> no 
#' significant trend). If 0 is not within ("trend - error" , "trend + error") 
#' but within ("trend - 2 x error" , "trend + 2 x error") the class is either
#' -1 (low significant negative trend) or +1 (low significant positive trend). 
#' If 0 is not even part of the larger interval the class is either -2 (highly 
#' significant negative trend) or +2 (highly significant positive trend), where
#' a positive trend describes the decrease of the trophic index.
#' 
#' The trend strength is the absolute value of the trend significance class 
#' multiplied by the actual trend and -1. Thus a trend strength >> 0 is a 
#' sigifinicant high decrease of trophic state and vice versa
#' 
#' @return 
#' The assessmentTable expended by two columns for the trend significance and
#' the trend strength
#' 
#' @export
#' 
trends_to_classes <- function(assessmentTable, trendType = "long"){
  columnNames <- if(trendType == "long"){
    c("trend_long", "error_long")
  } else {
    c("trend_short", "error_short")
  }
  
  assessmentTable[[paste0("trend_", trendType, "_significance")]] <- 
    sapply(1:nrow(assessmentTable), function(i){
      t <- assessmentTable[[columnNames[1]]][[i]]
      if(!is.na(t)){
        e <- assessmentTable[[columnNames[2]]][[i]]
        
        trend_class <- if(sum(c(t + e, t-e) > 0) == 1L){
          0
        } else if(sum(c(t + 2 * e, t-  2 *e) > 0) == 1L){
          1
        } else {
          2
        }
        if(t > 0){
          trend_class <- trend_class * -1
        }
        trend_class
      } else {
        NA
      }
    })
  
  assessmentTable[[paste0("trend_", trendType, "_strength")]] <- 
    abs(assessmentTable[[paste0("trend_", trendType, "_significance")]]) * 
    assessmentTable[[columnNames[1]]] * -1
  
  assessmentTable
}



