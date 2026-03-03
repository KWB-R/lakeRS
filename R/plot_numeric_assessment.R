#' Plot the numeric EO eutriphication assessment for one lake
#' 
#' Lake can be selected either by Name, ID or the rownumber in the assessment table
#' 
#' @param numericData The numeric assessment list created by 
#' [numericAssessment()]
#' @param lakeName,lakeID,rowNumber One of those need to be defined to select
#' the lake to be plotted
#' @param ylab Y axis lable
#' 
#' @importFrom grDevices dev.new rgb
#' @importFrom graphics axis layout points polygon
#' 
#' @export
#' 
plot_numeric_assessment <- function(
    numericData, lakeName = NULL, lakeID = NULL, rowNumber = NULL, ylab = "Index"
){
  df <- numericData$assessment
  r <-
    if(!is.null(rowNumber)){
      if("name" %in% colnames(df)){
        lakeName <- unique(df$name)
      }
      rowNumber
    } else if(!is.null(lakeName)){
      which(df$lakeName == lakeName)
    } else if(!is.null(lakeID)){
      which(df$lakeID == lakeID)
      if("name" %in% colnames(df)){
        lakeName <- unique(df$name)
      }
    }
  
  df_plot <- df[r,]
  status_columns <- grep(pattern = "_status$", x = colnames(df_plot))
  single_year_columns <- grep(pattern = "[0-9]$", colnames(df_plot))
  
  n_status_years <- length(status_columns)
  n_single_years <- length(single_year_columns)
  
  stats <- sapply(X = df[,single_year_columns], 
                  quantile, 
                  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE)
  
  dev.new(noRStudioGD = TRUE, width = 6, height = 4)
  
  if(!is.null(lakeName)){
    layout(mat = matrix(data = c(1,1,1,1,2,2,3,4), 
                        nrow = 2, ncol = 4, byrow = TRUE), 
           heights = c(0.2, 1))
    par(mar = c(0,0,0,0))
    plot(0,0,type ="n", xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n")
    text(x = 0, y = 0, lakeName, cex = 2)
  } else {
    layout(mat = matrix(data = c(1,1,2,3), 
                        nrow = 1, ncol = 4, byrow = TRUE), 
           heights = c(0.2, 1))
  }
  par(mar = c(4.1, 4.1, 1.1, 1.1))
  plot(x = 1:n_single_years, y = stats[5,], type = "n", 
       ylim =range(stats), ylab = ylab, xaxt = "n", xlab = "")
  polygon(x = c(1:n_single_years, n_single_years:1), 
          y = c(stats[1,], rev(stats[7,])), 
          col = rgb(208, 207, 253, 150, maxColorValue = 255, ), 
          border = NA)
  polygon(x = c(1:n_single_years, n_single_years:1), 
          y = c(stats[2,], rev(stats[6,])), 
          col = rgb(161, 160, 251, 100, maxColorValue = 255), 
          border = NA)   
  polygon(x = c(1:n_single_years, n_single_years:1), 
          y = c(stats[3,], rev(stats[5,])), 
          col = rgb(115, 112, 248, 50, maxColorValue = 255), 
          border = NA)
  lines(x = 1:n_single_years, y = stats[4,], col = rgb(0,3,226, 50,maxColorValue = 255), lwd = 2)
  
  points(x = 1:n_single_years,
         y = df_plot[,single_year_columns], 
         pch = 19, col = "gray40")
  lines(x = 1:n_status_years + n_single_years - n_status_years, 
        y = df_plot[,status_columns], 
        lwd = 2, lty = "dotted")
  
  axis(side = 1, 
       at = 1:n_single_years, 
       labels = substr(x = colnames(df_plot)[single_year_columns], start = 6, stop = 10),
       las = 2)
  
  trendLim <- 
    max(abs(range(
      c(df$trend_long - 1.96 * df$error_long,
        df$trend_long + 1.96 * df$error_long),
      na.rm = TRUE
    ))) * c(-1,1)
  plot(
    x = 0, y = 0, type = "n", ylim = trendLim,
    ylab = paste0(
      "Long-Term Trend (n = ", numericData$periods["longTerm"], ")"), 
    xlab = "", xaxt = "n"
  )
  abline(h = 0, lty = "dashed")
  points(x = 0, y = df_plot$trend_long, pch = 19, cex = 2)
  lines(
    x = c(0,0), 
    y = df_plot$trend_long + 1.96 * c(df_plot$error_long,-df_plot$error_long), 
    lwd = 3
  )
  
  trendLim <- 
    max(abs(range(
      c(df$trend_short - 1.96 * df$error_short,
        df$trend_short + 1.96 * df$error_short),
      na.rm = TRUE
    ))) * c(-1,1)
  plot(
    x = 0, y = 0, type = "n", ylim =  trendLim,
    ylab = paste0(
      "Short-Term Trend (n = ", numericData$periods["shortTerm"], ")"),
    xlab = "",xaxt = "n")
  abline(h = 0, lty = "dashed")
  points(x = 0, y = df_plot$trend_short, pch = 19, cex = 2)
  lines(
    x = c(0,0), 
    y = df_plot$trend_short + 1.96 * c(df_plot$error_short,-df_plot$error_short), 
    lwd = 3
  )
}
