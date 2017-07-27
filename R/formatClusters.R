#' @title Format Cluster Output
#' 
#' @description 
#' \code{formatCluster} formats the output of the cluster analysis.
#' 
#' @param clusterList The list containing all the clusters of the 
#'   hierarchical clustering.
#'   
#' @export
formatCluster <- function(clusterList) {
  
  # print out the number of clusters
  print(sprintf("There are %d clusters", length(clusterList)))
  
  # print the items in each cluster
  for(i in 1:length(clusterList)){
    print(paste(sprintf("#%d", i), paste(clusterList[[i]], collapse = ", ")))
  }
}