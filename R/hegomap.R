#' @title Heatmap of Ego Networks
#'
#' @description
#' \code{hegomap} produces a heatmap of ego networks and their characteristics
#'
#' @details
#' This function generates a heatmap of ego networks and their various
#' attributes from a data frame containing only numeric variables. These
#' numeric values are then scaled using a chosen method. The hierarchical
#' cluster analysis results used in the heatmap are also output. The 
#' heatmap is produced using the \link{heatmaply} package.
#' 
#' @param df a dataframe of ego network attributes containing only
#'   numeric values
#' @param scale.df A string indicating whether the data frame should be scaled, and if so,
#'   which method should be used. The options are "none", "basic", "normalize" and "percentize".
#'   If "none" is selected, then no scaling takes place.  If "basic" is selected, each
#'   column is reduced to the 0-1 interval, whithout changing the underlying distribution. 
#'   If "normalize" is selected, each column is normalized to have a mean of 0 and 
#'   standard deviation of 1. If "percentize" is selected, column values are transformed 
#'   into percentiles. This conserves the underlying distribution of the data if the data
#'   is not expected to be normally distributed.
#' @param link.method The agglomeration method to be used. See method in \link{hclust}.
#' @param dist.method The distance measure to be used. See method in \link{dist}.
#' @param nBootRep The number of bootstrap replications for the statistical analysis of 
#'   the hierarchical clustering. See \link{pvclust}.
#' @param mainTile The title of the heatmap.
#' @param xlabel The x axis label of the heatmap.
#' @param ylabel The y axis label of the heatmap.
#' @param xlabCex The font size of the elements on the x axis.
#' @param ylabCex The font size of the elements on the y axis.
#' @param heatmapMargins The size of the margins for the heatmap. See \link{heatmaply}.
#'
#' @export
#' 
#' @return a list containing the dendrogram of the hierarchical clustering,
#'   the results of the statistical analysis, the significant clusters of
#'   the statistical analysis and the heatmap.
#' @example 
#' \donttest{
#' hegomaps(df, scale.df = "basic", mainTitle = "Heatmap", 
#'            xlabel = "Chararacteritics", ylabel = "Networks",
#'            link.method = "single", dist.method = "euclidean")
#' }
hegomaps <- function(df, scale.df, link.method = "average", dist.method = "euclidean",
                     nBootRep = 1000, mainTitle  = "", xlabel, ylabel, xlabCex = 5,
                     ylabCex = 5, heatmapMargins = c(50, 50, 50, 100)){
  
  # check dataframe to make sure that it only contains numeric values
  if(FALSE %in% sapply(df, is.numeric))
    stop("Please only input dataframes that contain numeric values")
  
  # scale the data based on user selection
  if(scale.df == "basic")
    df <- scaleColumns(df)
  else if(scale.df == "normalize")
    df <- scale(df)
  else if(scale.df == "percentize")
    df <- percentize(df)
  
  # perform the cluster analysis on the variables of the networks
  results <- pvclust(df, method.hclust = link.method, method.dist = dist.method,
                     nboot = nBootRep)
  
  # create the dendrogram
  dend <- as.dendrogram(results)
  
  # get the significant clusters
  dendClusters <- pvpick(results)$clusters
  
  # create the heatmap
  hm <- heatmaply(df, Colv = dend, main = mainTitle,
                   seriate = "OLO",
                   xlab = xlabel,
                   ylab = ylabel,
                   margins = heatmapMargins,
                   scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue",
                                                                           high = "red",
                                                                           midpoint = 0.5),
                  cexRow = ylabCex,
                  cexCol = xlabCex)
  
  
  return(list(dend, dendClusters, results, hm))
}