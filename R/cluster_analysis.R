#' Find the number of clusters to use in kmeans
#' 
#' Based on the reduction of within cluster sum of squares
#' 
#' @param dpp The output of [dynamic_per_pixel()]
#' @param kMax Tha maximum number of clusters to be tested
#' @param minimum_clusterSize The minimum size of a cluster allowed. By default
#' 1% of all moving average pixels, need to be in a cluster. 
#' @param plot_result Logical
#' @param correlate_first If TRUE, Pearson correlation between pixels will be 
#' used for clustering instead of Index values. This results in a clustering 
#' based on different shapes of the dynamic. 
#' 
#' @details
#' The search for the best number of clusters is done by a maximum of 10 000 
#' pixels, which is assumed to be sufficient. If correlation is done first
#' those 10 000 pixels are correlated with 1000 pixels of the same data. 
#' Furthermore, the number of observerations (365 days) is reduced to every 
#' third day only, to lower process time. This way, correlation should take
#' less than 10 seconds.
#' 
#' 
#' @return This function plots the improvement values and returns the recommended
#' number of cluster to use
#' 
#' @importFrom stats kmeans cor
#' 
#' @export
#' 
best_nk <- function(
    dpp, kMax = 10, minimum_clusterSize = 0.01, plot_result = TRUE, correlate_first = FALSE
){
  y <- prepare_for_clustering(
    moving_averages_matrix = dpp$moving_averages[,-1], 
    correlate_first = correlate_first, 
    maxPixels = 10000
  )
  
  i <- 1
  kmeansOutput <- kmeans(t(y), centers = i, iter.max = 20, nstart = 2)
  wcss <-withinBetween <- kmeansOutput$betweenss / kmeansOutput$totss
  while(all(kmeansOutput$size > minimum_clusterSize * ncol(y))){
    if(i > kMax){
      break
    }
    i <- i + 1
    cat(paste0("Calculation of ", i, ifelse(i == 1, " cluster", " clusters"), " ... \n"))
    kmeansOutput <- kmeans(t(y), centers = i, iter.max = 20, nstart = 2)
   
    withinBetween <- kmeansOutput$betweenss / kmeansOutput$totss
    wcss <- c(wcss, withinBetween)
  }
  
  par(mar = c(4.1, 4.1, 4.1, 2.1))
  plot(
    x = 1:length(wcss), 
    y = wcss*100, 
    ylim = c(0,100),
    type = "l", 
    ylab = "%", 
    xlab = "Number of Clusters", 
    lwd = 2, main = "Variance Explained by Clusters")
  
  best_nCluster <- length(wcss)
  
  abline(v = best_nCluster)
  
  tp <- ifelse(
    abs(best_nCluster - par("xaxp")[2]) < abs(best_nCluster - par("xaxp")[1]), 
    yes = 2, 
    no = 4
  )
  text(x = best_nCluster, 
       y = 0, 
       labels = paste0("Recommended number of clusters = ", best_nCluster), 
       pos = tp
  )
  best_nCluster
}

#' cluster analysis (k-means) and map layer
#' 
#' @param dpp The output of [dynamic_per_pixel()]
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param k The number of Clusters
#' @param iter.max,nstart Arguments of [kmeans()]
#' @param correlate_first If TRUE, Pearson correlation between pixels will be 
#' used for clustering instead of Index values. This results in a clustering 
#' based on different shapes of the dynamic. 
#' @param whole_dynamic If TREU all 365 days average values will be used to 
#' compare pixels. Otherwise the number of days will be reduced if the number
#' of pixels is too high (> 100 000).
#' 
#' @return A list of the k-means output and a layer of clusted pixels
#' 
#' @importFrom stats kmeans cor
#' 
#' @export
#' 
pixel_clusters <- function(
    dpp, nc, k, iter.max = 20, nstart = 10, correlate_first = FALSE, 
    whole_dynamic = FALSE
){
  
  keep <- 1:365
  if(!whole_dynamic){
    proportionToRemove <- 100000 / ncol(dpp$moving_averages)
    i_d <- ceiling(1 / proportionToRemove)
    if(i_d > 10){
      i_d <- 10
    }
    keep <- seq(i_d, 365, i_d)
    dpp$moving_averages <- dpp$moving_averages[keep,]
    cat(paste0("due to large pixel number ", 365 - length(keep), 
               " days removed before clustering ... \n")
    )
  }
  
  y <- prepare_for_clustering(
    moving_averages_matrix = dpp$moving_averages[,-1], 
    correlate_first = correlate_first, 
    maxPixels = NULL
  )
  
  cat(paste0("clustering into ", k, " cluster ... \n"))
  kmeansOutput <- kmeans(
    x = t(y), centers = k, iter.max = iter.max, nstart = nstart
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
    "cluster_layer" = m_init,
    "days_included" = keep)
}

#' Pre-processing done before cluster analysis
#' 
#' @param moving_averages_matrix A matrix of p pixels as column and 365 days as
#' rows of the moving averages of one index as created by [dynamic_per_pixel()]
#' @param correlate_first If TRUE, Pearson correlation between pixels will be 
#' used for clustering instead of Index values. This results in a clustering 
#' based on different shapes of the dynamic.
#' @param maxPixels The maximum numbers of pixel used for cluster analysis.
#' Leave NULL to use all pixels.
#' 
#' @details
#' If correlate first, pixels are identified as outliers and removed if there 
#' not at least 5% other pixels that correlate at least 0.95. For example, given
#' 1000 correlated pixels, the evaluated pixels is not an outlier if it 
#' correlates to >= 50 pixels >= 0.95. 
#' 
#' 
#' @return A matrix either with data per pixels or correlation between pixels
#' used for following cluster analysis
#' 
#' @importFrom stats cor
#' 
#' @export
#' 
prepare_for_clustering <- function(
    moving_averages_matrix, correlate_first = FALSE, maxPixels = NULL
){
  ts_table <- signif(moving_averages_matrix[,-1], 3)
  
  na_values <- colSums(is.na(ts_table))
  ts_table <- ts_table[,(na_values == 0L)]
  nna <- sum(na_values > 0L)
  if(nna > 0L){
    warning(nna, " Pixels contained NA values and were removed",
            " prior to cluster analysis")
  }
  
  if(!is.null(maxPixels)){
    set.seed(1)
    ts_table <- ts_table[,sample(
      x = 1:ncol(ts_table), 
      size = min(maxPixels, ncol(ts_table))
    )]
  }
  
  if(correlate_first){
    ts_reference <- ts_table
    if(ncol(ts_reference) > 1000){
      set.seed(2)
      ts_reference <- ts_reference[,sample(1:ncol(ts_reference), size = 1000)]
    }
    cat(paste0("correlation of ", ncol(ts_table), " pixels ... \n"))
    y <- t(cor(x = ts_table, y = ts_reference, use = "pairwise.complete.obs"))
    
    
    # remove pixels that have a low correlation to almost "all" others
    # highCor <- apply(y, 2, quantile, p = 0.95)
    # )
    # cat(paste0(
    #   sum(highCor < 0.95), " pixels of ", ncol(y), " pixels identified as ",
    #   "outlier and removed ... \n")
    # )
    # 
    # # maxCor <- apply(y, 2, max)
    # # maxCor >= 0.99
    # 
    # sum(which(maxCor < 0.99) %in% which(highCor < 0.95))
    # y <- round(y[,highCor >= 0.95], 5)
    y <- round(y, 5)
  } else {
    y <- ts_table
  }
  y
}



