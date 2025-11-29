#' Plot the numeric EO eutriphication assessment for one lake
#' 
#' Lake can be selected either by Name, ID or the rownumber in the assessment table
#' 
#' @param class_assessment The numeric assessment list created by 
#' [EO_assessment_numeric]
#' @param lakeNames,lakeIDs,rowNumbers One or more of those can be defined as 
#' character vector for lakeNames and lakeIDs or numeric vectors for rowNUmbers.
#' Different filters can be combined
#' 
#' @importFrom grDevices rgb
#' @importFrom graphics axis rect
#' 
#' @export
#' 
plot_class_assessment <- function(
    class_assessment, lakeNames = NULL, lakeIDs = NULL, rowNumbers = NULL
){
  
  # color table
  ct <- rescale_classColors(nClass = class_assessment$nClass)
  
  df <- class_assessment$assessment

  rowFilter <- 
    c(which(df$lakeName %in% lakeNames | df$lakeID %in% lakeIDs), rowNumbers)
  
  if(length(rowFilter) == 0L){
    stop("At least one of 'lakeNames', 'lakeIDs','rowNumbers' must be specified.")
  }
  
  df_plot <- df[unique(rowFilter),]
  n_lakes <- nrow(df_plot)
  
  class_cols <- grep(pattern = "_class$", colnames(df_plot))
  #color_cols <- grep(pattern = "_color$", colnames(df_plot))
  years <- substr(colnames(df_plot)[class_cols], 6, 9)
  n_years <- length(class_cols)
  n_lakes <- nrow(df_plot)
  par(mar = c(4.1, 10.1, 1.1,1.1))
  plot(x = 0, y = 0, 
       xlim = c(0,n_years) + 0.5, 
       ylim = c(0,n_lakes) + 0.5, xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  axis(side = 1, at = 1:n_years, labels = years, las = 2, tick = FALSE)
  axis(side = 2, at = rev(1:n_lakes), 
       labels = paste0(df_plot$lakeName, "\n" , "(", df_plot$lakeID, ")"), las = 1)
  
  df_reverse <- df_plot[nrow(df_plot):1,]
  
  for(i in 1:n_lakes){
    for(j in class_cols){
      waterClass <- df_reverse[i,j]
      rect(xleft = j - class_cols[1] + 0.5, xright = j - class_cols[1] + 1.5, 
           ybottom = i - 0.5, ytop = i + 0.5, 
           col =  ct$color[ct$class == waterClass])
      text(x = j + 1 - class_cols[1], y = i, labels = waterClass, font = 2)
    }
  }
}

#' Rescale class colors
#' 
#' @param nClass Number of Classes
#' 
#' @importFrom grDevices rgb col2rgb
#' 
rescale_classColors <- function(nClass){
  df_out <- lakeRS::tenClassColors
  df_out$class <- as.numeric(df_out$class)
  
  if(nClass < 10){
    df_out <- df_out[round(seq(from = 1, to = 10, length.out = nClass)),]
  } else if(nClass > 10){
    definedClasses <- tableClasses <- nrow(df_out)
    i <- 1
    while(nClass > definedClasses){
      betweenClass <- mean(df_out$class[i:(i+1)])
      betweenColor <- apply(col2rgb(df_out$color[1:2]), MARGIN = 1, mean)
      betweenColor <- rgb(
        red = betweenColor[1], 
        green = betweenColor[2], 
        blue = betweenColor[3], 
        maxColorValue = 255)
      df_out <- rbind(
        df_out, 
        data.frame("class" = betweenClass, "color" = betweenColor)
      )
      definedClasses <- definedClasses + 1
      i <- i + 1
      
      if(i == tableClasses){
        df_out <- df_out[order(df_out$class),]
        tableClasses <- nrow(df_out)
        i <- 1
      }
    }
    df_out <- df_out[order(df_out$class),]
  }
  df_out$class <- 1:nClass
  
  df_out
}
