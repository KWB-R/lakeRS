#' Estimate a suitable number of k-means clusters
#'
#' Runs k-means clustering for increasing numbers of clusters and recommends the
#' largest number that still satisfies a minimum cluster-size criterion. The
#' decision is based on the proportion of variance explained by the clustering.
#'
#' @param pixelDynamic A list or data-frame-like object of pixel-level annual
#'   dynamics, usually `pixelDynamics` from [dynamic_per_pixel()]. Each pixel is
#'   expected to contain a 365-day moving-average series.
#' @param kMax Integer. Maximum number of clusters to test. Default is `10`.
#' @param minimum_clusterSize Numeric. Minimum allowed cluster size expressed as
#'   a proportion of all clustered pixels. Default is `0.01`.
#' @param correlate_first Logical. If `TRUE`, pixels are clustered by their
#'   correlation profiles rather than by raw index values.
#'
#' @return Integer giving the recommended number of clusters. The function also
#'   draws a diagnostic plot of explained variance by number of clusters.
#'
#' @details At most 10,000 pixels are sampled before testing cluster counts. If
#'   `correlate_first = TRUE`, the selected pixels are correlated with at most
#'   1,000 reference pixels. See [prepare_for_clustering()] for details on NA
#'   handling and optional correlation pre-processing.
#'
#' @importFrom stats kmeans cor
#' @export
#' 
best_nk <- function(
    pixelDynamic, kMax = 10, minimum_clusterSize = 0.01, 
    correlate_first = FALSE
){
  y <- prepare_for_clustering(
    moving_averages_matrix = do.call(cbind, pixelDynamic), 
    correlate_first = correlate_first, 
    maxPixels = 10000
  )
  
  i <- 1
  kmeansOutput <- kmeans(t(y), centers = i, iter.max = 20, nstart = 2)
  wcss <- withinBetween <- kmeansOutput$betweenss / kmeansOutput$totss
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

#' Cluster pixel dynamics with k-means
#'
#' Performs k-means clustering on pixel-level moving-average dynamics and returns
#' cluster assignments, cluster centers, and selected k-means diagnostics.
#'
#' @param pixelDynamic A list or data-frame-like object of pixel dynamics,
#'   usually `pixelDynamics` from [dynamic_per_pixel()].
#' @param k Integer. Number of clusters.
#' @param iter.max Integer. Maximum number of k-means iterations.
#' @param nstart Integer. Number of random starts passed to [stats::kmeans()].
#' @param correlate_first Logical. If `TRUE`, clustering uses pixel correlation
#'   profiles instead of raw moving-average values.
#' @param whole_dynamic Logical. If `FALSE`, the number of days used for
#'   clustering may be reduced for large pixel sets. If `TRUE`, all 365 days are
#'   used.
#'
#' @return A list with `clusterVector`, `clusterCenter`, `kmeansOut`, and
#'   `days_included`.
#'
#' @importFrom stats kmeans cor
#' @export
pixel_clusters <- function(
    pixelDynamic, k, iter.max = 20, nstart = 10, correlate_first = FALSE, 
    whole_dynamic = FALSE
){
  
  
  keep <- 1:365
  if(!whole_dynamic){
    proportionToRemove <- 100000 / length(pixelDynamic)
    i_d <- ceiling(1 / proportionToRemove)
    if(i_d > 10){
      i_d <- 10
    }
    keep <- seq(i_d, 365, i_d)
    if(i_d > 1L){
      cat(paste0("Due to large pixel number ", 365 - length(keep), 
                 " days removed before clustering ... \n")
      )
    } 
  }
  
  y <- prepare_for_clustering(
    moving_averages_matrix = do.call(cbind, pixelDynamic)[keep,], 
    correlate_first = correlate_first, 
    maxPixels = NULL
  )
  
  cat(paste0("clustering into ", k, " cluster ... \n"))
  kmeansOutput <- kmeans(
    x = t(y), centers = k, iter.max = iter.max, nstart = nstart
  )
  
  list(
    "clusterVector" = kmeansOutput$cluster,
    "clusterCenter" = t(kmeansOutput$centers),
    "kmeansOut" = kmeansOutput[-grep(
      pattern = "cluster|centers", 
      x = names(kmeansOutput))
    ],
    "days_included" = keep)
}

#' Prepare pixel dynamics for clustering
#'
#' Cleans and optionally transforms a pixel-by-day moving-average matrix before
#' k-means clustering.
#'
#' @param moving_averages_matrix Matrix or data frame with days as rows and
#'   pixels as columns. The first column is dropped before processing, matching
#'   outputs that include a day-of-year column.
#' @param correlate_first Logical. If `TRUE`, pixels are represented by their
#'   Pearson correlation with a reference set of pixels.
#' @param maxPixels Optional integer. Maximum number of pixels to retain. If not
#'   `NULL`, a reproducible random sample is drawn.
#'
#' @return A numeric matrix used as input for clustering. Columns represent
#'   pixels when `correlate_first = FALSE`; otherwise the matrix contains rounded
#'   correlation profiles.
#'
#' @details Values are rounded with [signif()] before clustering. Depending on
#'   the NA pattern, the function removes either rows with missing values or
#'   pixels with missing values and reports removals via warnings. If correlation
#'   pre-processing is requested, at most 1,000 reference pixels are sampled.
#'
#' @importFrom stats cor
#' @export
prepare_for_clustering <- function(
    moving_averages_matrix, correlate_first = FALSE, maxPixels = NULL
){
  ts_table <- signif(moving_averages_matrix[,-1], 3)
  
  na_values <- colSums(is.na(ts_table))
  if(all(na_values) > 0){
    na_values2 <- rowSums(is.na(ts_table))
    ts_table <- ts_table[(na_values2 == 0L),]
    nna2 <- sum(na_values2 > 0L)
    if(nna2 > 0L){
      warning(nna2, " Days contained NA values only and were removed",
              " prior to cluster analysis")
    }
    na_values <- colSums(is.na(ts_table))
  } else {
    ts_table <- ts_table[,(na_values == 0L)]
    nna <- sum(na_values > 0L)
    if(nna > 0L){
      warning(nna, " Pixels contained NA values and were removed",
              " prior to cluster analysis")
    }
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



