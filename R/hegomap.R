#' @title Heatmap of Ego Networks
#'
#' @description
#' \code{hegomap} produces a heatmap of ego networks and their characteristics
#'
#' @details
#' This function generates a heatmap of ego networks and their various
#' attributes from a data frame containing only numeric variables. These
#' numeric values are then scaled using a chosen method. The hierarchical
#' cluster analysis results used in the heatmap are also printed to the
#' console. The heatmap is produced using the \link{heatmaply} package.
#' 
#' @param df a dataframe of ego network attributes containing only
#'   numeric values
#' @param printCluster a boolean value to determine if the cluster analysis
#'   should be printed to the console.
#' @param scale.df A string indicating whether the data frame should be scaled, and if so,
#'   which method should be used. The options are "none", "basic", "normalize" and "percentize".
#'   If "none" is selected, then no scaling takes place.  If "basic" is selected, each
#'   column is reduced to the 0-1 interval, whithout changing the underlying distribution. 
#'   If "normalize" is selected, each column is normalized to have a mean of 0 and 
#'   standard deviation of 1. If "percentize" is selected, column values are transformed 
#'   into percentiles. This conserves the underlying distribution of the data if the data
#'   is not expected to be normally distributed. Defaults to "none".   
#' @export
#' @return a list containing the dendrogram with the statistical analysis of the clusters,
#'   the results of the said statistical analysis and the heatmap
#' @author Phil Boileau <philippe.boileau@mail.concordia.ca>
#' @example
#' 
hegomaps <- function(df, scale.df, link.method = "average", dist.method = "euclidean",
                     nBootRep = 1000, cexLabels = 1, cexAU = 1, pAlpha = 0.95,
                     mainTitle, xlabel, ylabel, xlabFontSize = 5, ylabFontSize = 5,
                     heatmapMargins = c(50, 50, 50, 100), printCluster = FALSE){
  
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
  dend <- as.dendrogram(results)

  
  # create the heatmap
  hm <- heatmaply(df, Colv = dend, main = mainTitle,
            xlab = xlabel,
            ylab = ylabel,
            margins = heatmapMargins,
            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.5),
            cexRow = ylabFontSize,
            cexCol = xlabFontSize)
  
  return(list(dend, results, hm))
}