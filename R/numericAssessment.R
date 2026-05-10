#' Assess lake status and relative trends from yearly index values
#'
#' Adds moving-average status and relative short- and long-term trend estimates 
#' to a wide table of yearly index values.
#'
#' @param yearly_spread A data frame with lake or pixel identifiers and yearly
#'   index columns named `year_<year>`
#' @param statusYears Integer. Number of years used for moving-average status.
#'   Default is `3`.
#' @param shortTermYears Integer. Number of years used for the short-term trend.
#'   Default is `3`.
#' @param longTermYears Integer. Number of years used for the long-term trend.
#'   Default is `10`.
#'
#' @return A list with `assessment` and `periods`. `assessment` is the input data
#'   frame extended by status and trend columns where possible. `periods` records
#'   the three period lengths used.
#'
#' @details In order to rule out systematic errors based on weather conditions 
#'   in a year, different hardware or algorithms, the difference between the 
#'   overall median value of all lakes (or pixels) and years and the yearly 
#'   median value of all lakes (or pixels) is added to each index per year before 
#'   the trends are calculated. Thus, the trends of one lake (or pixel) are 
#'   relative compared to the other lakes (or pixels) of the table. The trend is 
#'   calculated as linear regression between previous years and adjusted NDI 
#'   values.
#'   For a reliable trend assessment the number of lakes (or pixels) needs to 
#'   be large enough (n>10).
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

#' Calculate moving-average status over multiple years
#'
#' Computes a trailing moving average of yearly index columns for each lake or
#' pixel. The function is an internal helper used by [numericAssessment()].
#'
#' @param yearly_spread A data frame with one or more columns whose names start
#'   with `year_`. Rows represent lakes or pixels.
#' @param n_years Integer. Number of consecutive years used for each moving
#'   average window. Default is `3`.
#'
#' @return A list with `status`, a matrix of moving-average values, and `nYears`,
#'   the window length used. Status columns are named after the final year of
#'   each window and suffixed with `_status`.
#'
#' @details Missing values are not removed (`mean()` is called without
#'   `na.rm = TRUE`), so a missing value in any year of a window yields `NA` for
#'   that row and status year.
#'
#' @keywords internal
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

#' Calculate adjusted linear trends over recent years
#' 
#' @param yearly_spread A data frame of lakes or pixels as row and the yearly 
#' index values as columns
#' @param tyears The number of years used for the trend analysis.
#' @param ttype Character scalar, either `"short"` or `"long"`. Used to name the
#'   output columns.
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
#' Only rows with complete data in all year columns are used to estimate
#' the global and yearly medians for the bias correction. Individual row trends
#' are returned as `NA` if any value in the selected trend window is missing.
#' 
#' @return A list with `trends`, `periodYear`, `overallMedian`, and
#'   `yearlyMedians`. `trends` contains the slope and the standard error of the
#'   slope for each row.
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


#' Convert numeric trend estimates to significance classes
#'
#' Adds categorical trend-significance and trend-strength columns to an
#' assessment table containing trend estimates and standard errors.
#'
#' @param assessmentTable A data frame, typically `numericAssessment(...)$assessment`,
#'   containing trend and error columns for the selected `trendType`.
#' @param trendType Character scalar. Either `"long"` or `"short"`; selects
#'   `trend_long`/`error_long` or `trend_short`/`error_short`.
#'
#' @return The input data frame extended by `trend_<trendType>_significance` and
#'   `trend_<trendType>_strength`.
#'
#' @details Significance classes are `0` if zero lies within ±1 standard error,
#'   `±1` if zero lies outside ±1 but inside ±2 standard errors, and `±2` if zero
#'   lies outside ±2 standard errors. Positive trend estimates are converted to
#'   negative classes, so class sign follows the package's interpretation that a
#'   negative numerical trend indicates improvement of the trophic index.
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



