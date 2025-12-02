#' Find the number of clusters to use in kmeans
#' 
#' Based on the reduction of within cluster sum of squares
#' 
#' @param ts_table A table of timeseries data per pixel created by 
#' [dynamic_per_pixel()]
#' @param kMax Tha maximum number of clusters to be tested
#' @param improvement_objective The improvement of within cluster sum of squares
#' (WCSS) by addition of one cluster in %.
#' @param plot_result Logical
#' 
#' @return This function plots the improvement values and returns the recommended
#' number of cluster to use
#' 
#' @importFrom stats kmeans
#' 
#' @export
#' 
best_nk <- function(
    ts_table, kMax = 10, improvement_objective = 10, plot_result = TRUE
){
  ts_table <- ts_table[,-1]
  if(ncol(ts_table) > 10000){
    set.seed(1)
    ts_table <- ts_table[,sample(1:ncol(ts_table), size = 10000)]
  }
  
  na_values <- sapply(ts_table, function(x){any(is.na(x))})
  remove_pixel <- which(na_values)
  if(length(remove_pixel) > 0L){
    ts_table <- ts_table[,-remove_pixel]
    warning(length(remove_pixel), " Pixels contained NA values and were removed",
            " prior to cluster analysis")
  }
  
  wcss <- c()
  for(i in 1:kMax){
    cat(paste0("Calculation of ", i, ifelse(i == 1, " cluster", " clusters"), " ... \n"))
    kmeansOutput <- kmeans(t(ts_table), centers = i, iter.max = 20, nstart = 10)
    wcss <- c(wcss, kmeansOutput$tot.withinss)
  }
  wcss_improve <- -(diff(wcss) / wcss[1:(length(wcss) - 1)] * 100)
  plot(
    x = 2:length(wcss), 
    y = wcss_improve, 
    ylim = c(0,max(wcss_improve)),
    type = "l", 
    ylab = "Within Cluster Sum of Squares Improvement in %", 
    xlab = "Number of Clusters", 
    lwd = 2, main = "Identification of best number of clusters")
  
  best_nCluster <- max(seq_along(wcss_improve)[wcss_improve > improvement_objective]) + 1
  abline(v = best_nCluster)
  abline(h = improvement_objective)
  text(x = best_nCluster, y = 0, 
       labels = paste0("Recommended number of clusters = ", best_nCluster), pos = 4)
  best_nCluster
}



#' cluster analysis (k-means) and map layer
#' 
#' @param dpp The output of [dynamic_per_pixel()]
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param k The number of Clusters
#' @param iter.max,nstart Arguments of [kmeans()]
#' 
#' @return A list of the k-means output and a layer of clusted pixels
#' 
#' @importFrom stats kmeans
#' 
#' @export
#' 
pixel_clusters <- function(
    dpp, nc, k, iter.max = 20, nstart = 10
){
  na_values <- sapply(dpp$moving_averages, function(x){any(is.na(x))})
  remove_pixel <- which(na_values)
  if(length(remove_pixel) > 0L){
    dpp$moving_averages <- dpp$moving_averages[,-remove_pixel]
    warning(length(remove_pixel), " Pixels contained NA values and were removed",
            " prior to cluster analysis")
  }
  # rounding data points to smooth out differences of very similar data points
  x <- signif(dpp$moving_averages[,-1], 3)
  kmeansOutput <- kmeans(
    t(x), centers = k, iter.max = iter.max, nstart = nstart
  )
  
  m_init <- matrix(data = 0, nrow = length(nc$y), ncol = length(nc$x))
  up <- dpp$raster_location
  for(i in seq_along(kmeansOutput$cluster)){
    m_init[up$i_row[up$pixel == names(kmeansOutput$cluster)[i]],
           up$i_col[up$pixel == names(kmeansOutput$cluster)[i]]] <- 
      kmeansOutput$cluster[i]
  }
  
  list(
    "kmeans" = kmeansOutput,
    "cluster_layer" = m_init)
}


