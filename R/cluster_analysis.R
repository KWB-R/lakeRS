#' Find the number of clusters to use in kmeans
#' 
#' Based on the reduction of within cluster sum of squares
#' 
#' @param dpp The output of [dynamic_per_pixel()]
#' @param kMax Tha maximum number of clusters to be tested
#' @param explanation_objective The a threshold between 0 and 1 for the ratio 
#' between Within-Cluster sum of squares and Between-Cluster sum of squares.
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
    dpp, kMax = 10, explanation_objective = 0.98, plot_result = TRUE, correlate_first = FALSE
){
  y <- prepare_for_clustering(
    moving_averages_matrix = dpp$moving_averages[,-1], 
    correlate_first = correlate_first, 
    maxPixels = 10000
  )
  
  wcss <- c()
  withinBetween <- 0
  i <- 1
  while(withinBetween < explanation_objective){
    if(i > kMax){
      break
    }
    cat(paste0("Calculation of ", i, ifelse(i == 1, " cluster", " clusters"), " ... \n"))
    
    kmeansOutput <- kmeans(t(y), centers = i, iter.max = 20, nstart = 2)
    
    withinBetween <- kmeansOutput$betweenss / kmeansOutput$totss
    wcss <- c(wcss, withinBetween)
    i <- i + 1
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
  abline(h = explanation_objective * 100, lty = "dotted")
  
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
    i_d <- round(1 / proportionToRemove)
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
    y <- round(y, 5)
  } else {
    y <- ts_table
  }
  y
}



