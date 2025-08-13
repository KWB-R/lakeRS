# projec path and data path
pp <- "Y:/iGB/Projects/AD4GD"
dp <- "Exchange/01_data"

ndtri_folders <- "03_output/netCDF_ndtri"

if(FALSE){
  EO_assessment_ndtri <- read.csv(
    file.path(pp, dp, ndtri_folders, "_all_lakes", "Ndtri_Berlin.csv"), 
    header = TRUE, sep = ";")
  EO_assessment_class <- read.csv(
    file.path(pp, dp, ndtri_folders, "_all_lakes", "Ndtri_Classes_Berlin.csv"), 
    header = TRUE, sep = ";")
  
  # this was only done to create the data tables
  if(FALSE){
    all_folders <- dir(file.path(pp, dp, ndtri_folders))
    
    df_ndtri <- lapply(
      X = all_folders, 
      FUN = reshape_ndtri_table,
      path = file.path(pp, dp, ndtri_folders),
      value_column = "Modus_NDTrI", 
      condition_column = "pixel_quality", 
      condition_value = 0.8
    )
    
    df_ndtri <- do.call(rbind, df_ndtri)
    
    # only keep lakes with data for all years to get status, trends and class
    # boundaries
    every_year <- apply(df_ndtri[,-c(1,2)], MARGIN = 1, function(x){
      !any(is.na(x))
    })
    df_complete <- df_ndtri[every_year,]
    
    # NDTrI Status and Trends ----------------------------------------------------
    ndtri_status <- get_ndtri_status(yearly_spread = df_complete, n_years = 3)
    ndtri_trend <- get_ndtri_trend(yearly_spread = df_complete, short_term = 3, long_term = 7)
    
    EO_assessment_ndtri <- do.call(cbind, list(
      df_complete, 
      ndtri_status, 
      ndtri_trend)
    )
    
    write.table(
      x = EO_assessment_ndtri, 
      file = file.path(
        pp, dp, ndtri_folders, "_all_lakes", paste0("Ndtri_Berlin", ".csv")), 
      sep = ";", dec = ".", row.names = FALSE)
    
    # Class results
    ndtri_classes <- get_ndtri_classes(yearly_spread = df_complete, n_classes = 10)
    
    EO_assessment_class <- do.call(cbind, list(
      df_complete, 
      ndtri_classes$yearly_classes,  
      ndtri_classes$yearly_colors))
    
    write.table(
      x =  EO_assessment_class, 
      file = file.path(
        pp, dp, ndtri_folders, "_all_lakes", 
        paste0("Ndtri_Classes_Berlin", ".csv")), 
      sep = ";", dec = ".", row.names = FALSE)
  }
  
  
  # Plot results ----------------------------------------------------
  plot_ndtri_status(EO_assessment = EO_assessment_ndtri, lake_names = "Lietzensee")
  
  
  rows_to_plot <- 15:30
  par(mar = c(4.1, 11.1, 1.1, 1.1))
  par(cex = 0.8)
  plot_ndtri_classes(
    EO_assessment = EO_assessment_class, 
    rows_to_plot = rows_to_plot
  )
  
}

# function        
reshape_ndtri_table <- function(
    path, folder, value_column, 
    condition_column = NULL, condition_value = NULL
){
  df_in <- read.table(
    file = file.path(path, folder, "seasonal_data.csv"), 
    header = TRUE, 
    sep = ";", 
    dec = "."
  )
  
  df_out <- data.frame(
    "lake_name" = strsplit(folder, "_")[[1]][1],
    "lake_id" =  strsplit(folder, "_")[[1]][2])
  
  for(i_year in 1:nrow(df_in)){
    df_out[df_in[i_year,1]] <- 
      if(!is.null(condition_column)){
        if(df_in[i_year, condition_column] == condition_value){
          df_in[i_year, value_column]
        } else {
          NA
        }
      } else {
        df_in[i_year, value_column]
      }
  }
  
  df_out
}

get_ndtri_status <- function(yearly_spread, n_years = 3){
  first_year_column <- min(grep(pattern = "^Year",colnames(yearly_spread)))
  last_year_column <- ncol(yearly_spread)
  
  df_out <- sapply(first_year_column:(last_year_column - n_years + 1), function(i){
    x <- yearly_spread[,i:(i + n_years - 1)]
    apply(x, 1, mean)
  })
  colnames(df_out) <- 
    paste0(colnames(yearly_spread)[(first_year_column + n_years - 1):last_year_column], "_status")
  cbind(df_out, "years_status" =  c(n_years))
}

get_ndtri_trend <- function(yearly_spread = yearly_spread, short_term = 3, long_term = 7){
  single_year_columns <- grep(pattern = "^Year", 
                              x = colnames(yearly_spread))
  
  mat <- as.matrix(yearly_spread[,single_year_columns])
  overall_median <- median(mat)
  yearly_medians <- apply(mat, 2, median)
  yearly_shift <- overall_median - yearly_medians
  
  if(short_term > ncol(mat)){
    short_term <- ncol(mat)
  }
  if(long_term > ncol(mat)){
    long_term <- ncol(mat)
  }
  
  short_term_columns <- (ncol(mat) - short_term:1 + 1)
  long_term_columns <- (ncol(mat) - long_term:1 + 1)
  
  short_term_mat <- mat[,short_term_columns]
  long_term_mat <- mat[,long_term_columns]
  
  
  t(sapply(1:nrow(mat), function(i){
    short_term_df <- data.frame(
      "Year" = colnames(short_term_mat),
      "NDTrI" = short_term_mat[i,],
      "NDTrI_adjusted" = short_term_mat[i,] + yearly_shift[short_term_columns],
      "years_passed" = -(short_term - 1):0
    )
    long_term_df <- data.frame(
      "Year" = colnames(long_term_mat),
      "NDTrI" = long_term_mat[i,],
      "NDTrI_adjusted" = long_term_mat[i,] + yearly_shift[long_term_columns],
      "years_passed" = -(long_term - 1):0
    )
    
    st <- summary(lm(NDTrI_adjusted ~ years_passed, data = short_term_df))
    lt <- summary(lm(NDTrI_adjusted  ~ years_passed, data = long_term_df))
    
    c("trend_short" = st$coefficients[2,1],
      "error_short" = st$coefficients[2,2],
      "years_short" = length(short_term_columns),
      "trend_long" = lt$coefficients[2,1],
      "error_long" = lt$coefficients[2,2],
      "years_long" = length(long_term_columns))
  }))
}

get_ndtri_classes <- function(yearly_spread, n_classes = 10){
  
  color_table <- data.frame(
    "class" = 1:10,
    "color" = c(
      rgb(94,79,162, maxColorValue = 255),
      rgb(50,136,189, maxColorValue = 255),
      rgb(102,194,164, maxColorValue = 255),
      rgb(171,221,164, maxColorValue = 255),
      rgb(230,245,152, maxColorValue = 255),
      rgb(254,224,139, maxColorValue = 255),
      rgb(253,174,94, maxColorValue = 255),
      rgb(244,109,67, maxColorValue = 255),
      rgb(213,62,79, maxColorValue = 255),
      rgb(158,1,66, maxColorValue = 255)
    ))
  
  last_year_column <- ncol(yearly_spread)
  
  yearly_classes <- sapply(grep(pattern = "^Year", colnames(yearly_spread)), function(i){
    year_i <- yearly_spread[,i]
    breaks <- c(-1,
                seq(quantile(year_i, probs = 0.05), 
                    quantile(year_i, probs = 0.95), length.out = n_classes - 1), 
                1)
    cut(x = year_i, breaks = breaks, labels = color_table$class, include.lowest = TRUE)
  })
  
  colnames(yearly_classes) <- 
    paste0(colnames(yearly_spread)[grep(pattern = "^Year", colnames(yearly_spread))], "_class")
  
  yearly_colors <- sapply(grep(pattern = "^Year", colnames(yearly_spread)), function(i){
    year_i <- yearly_spread[,i]
    breaks <- c(-1,
                seq(quantile(year_i, probs = 0.05), 
                    quantile(year_i, probs = 0.95), length.out = n_classes - 1), 
                1)
    cut(x = year_i, breaks = breaks, labels = color_table$color, include.lowest = TRUE)
  })
  colnames(yearly_colors) <- 
    paste0(colnames(yearly_spread)[grep(pattern = "^Year", colnames(yearly_spread))], "_color")
  
  list("yearly_classes" = yearly_classes, "yearly_colors" = yearly_colors)
}


plot_ndtri_classes <- function(EO_assessment, rows_to_plot){
  df_plot <-  EO_assessment[rows_to_plot,]
  
  class_cols <- grep(pattern = "_class$", colnames(df_plot))
  color_cols <- grep(pattern = "_color$", colnames(df_plot))
  years <- substr(colnames(df_plot)[class_cols], 6, 9)
  n_years <- length(class_cols)
  n_lakes <- nrow(df_plot)
  par(mar = c(4.1, 11.1, 1.1,1.1))
  plot(x = 0, y = 0, 
       xlim = c(0,n_years) + 0.5, 
       ylim = c(0,n_lakes) + 0.5, xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  axis(side = 1, at = 1:n_years, labels = years, las = 2, tick = FALSE)
  axis(side = 2, at = rev(1:n_lakes), 
       labels = paste0(df_plot$lake_name, "\n" , "(", df_plot$lake_id, ")"), las = 1)
  
  df_reverse <- df_plot[nrow(df_plot):1,]
  for(i in 1:n_lakes){
    for(j in 1:n_years){
      rect(xleft = j - 0.5, xright = j + 0.5, ybottom = i - 0.5, ytop = i + 0.5, 
           col = df_reverse[i,color_cols[j]])
      text(x = j, y = i, labels = df_reverse[i,class_cols[j]], font = 2)
    }
  }
  
}

plot_ndtri_status <- function(EO_assessment, lake_names = NULL, lake_ids = NULL, row_numbers = NULL){
  r <- c()
  if(!is.null(row_numbers)){
    r <- c(r, row_numbers)
  }
  r <- c(
    r, 
    get_rows_by_info(
      df = EO_assessment, 
      name_column = "lake_name", 
      id_column = "lake_id", 
      lake_names = lake_names, 
      lake_ids = lake_ids)
  )
  df_plot <- EO_assessment[r,]
  status_columns <- grep(pattern = "[0-9]_status$", x = colnames(df_plot))
  single_year_columns <- grep(pattern = "[0-9]$", colnames(df_plot))
  
  
  n_status_years <- length(status_columns)
  n_single_years <- length(single_year_columns)
  
  stats <- sapply(X = EO_assessment[,single_year_columns], 
                  quantile, 
                  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1))
  
  dev.new(noRStudioGD = TRUE, width = 6, height = 4)
  
  layout(mat = matrix(data = c(1,1,1,1,2,2,3,4), 
                      nrow = 2, ncol = 4, byrow = TRUE), 
         heights = c(0.2, 1))
  par(mar = c(0,0,0,0))
  plot(0,0,type ="n", xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n")
  text(x = 0, y = 0, df_plot$name, cex = 2)
  
  par(mar = c(4.1, 4.1, 1.1, 1.1))
  plot(x = 1:n_single_years, y = stats[5,], type = "n", 
       ylim =range(stats), ylab = "NDTrI", xaxt = "n", xlab = "")
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
         pch = 19)
  lines(x = 1:n_status_years + n_single_years - n_status_years, 
        y = df_plot[,status_columns], 
        lwd = 1, lty = "dotted")
  
  axis(side = 1, 
       at = 1:n_single_years, 
       labels = colnames(df_plot)[single_year_columns], 
       las = 2)
  
  plot(x = 0, y = 0, ylab = "Langzeittrend", type = "n", xlab = "", xaxt = "n",
       ylim = range(EO_assessment$trend_long))
  abline(h = 0, lty = "dashed")
  points(x = 0, y = df_plot$trend_long, pch = 19, cex = 2)
  lines(x = c(0,0), 
        y = df_plot$trend_long + 1.96 * c(df_plot$error_long,-df_plot$error_long), 
        lwd = 3)
  
  plot(x = 0, y = 0, ylab = "Kurzzeittrend", type = "n", xlab = "", xaxt = "n",
       ylim = range(EO_assessment$trend_short))
  abline(h = 0, lty = "dashed")
  points(x = 0, y = df_plot$trend_short, pch = 19, cex = 2)
  lines(x = c(0,0), 
        y = df_plot$trend_short + 1.96 * c(df_plot$error_short,-df_plot$error_short), 
        lwd = 3)
}

get_rows_by_info <- function(df, name_column = NULL, id_column = NULL, lake_names = NULL, lake_ids = NULL){
  r <- c()
  if(!is.null(lake_names)){
    missing <- which(!(lake_names %in% df[[name_column]]))
    if(length(missing) > 0){
      warning(paste("No lake/lakes called", lake_names[missing], "in data frame."))
    } 
    r <- c(r, which(df[[name_column]] %in% lake_names))
  } 
  if(!is.null(lake_ids)){
    missing <- which(!(lake_ids %in% df[[id_column]]))
    if(length(missing) > 0){
      warning(paste("No lake ID/IDs", lake_names[missing], "in data frame."))
    } 
    r <- c(r, which(df[[name_column]] %in% lake_names))
  }
  r 
}

