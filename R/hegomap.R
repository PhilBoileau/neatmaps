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
#' @param scale A string indicating whether the data frame should be scaled, and if so,
#'   which method should be used. The options are "none", "basic", "normalize" and "percentize".
#'   If "none" is selected, then no scaling takes place.  If "basic" is selected, each
#'   column is reduced to the 0-1 interval, whithout changing the underlying distribution. 
#'   If "normalize" is selected, each column is normalized to have a mean of 0 and 
#'   standard deviation of 1. If "percentize" is selected, column values are transformed 
#'   into percentiles. This conserves the underlying distribution of the data if the data
#'   is not expected to be normally distributed. Defaults to "none".   
#' @export
#' @return an interactive heatmap, as well as a hierarchal clustering 
#'   analysis on the ego networks and on their attributes
#' @author Phil Boileau <philippe.boileau@mail.concordia.ca>
#' @example
#' 
hegomaps <- function(df, printCluster = FALSE){
  
  # check dataframe to make sure that it only contains numeric values
  if(FALSE %in% sapply(df, is.numeric))
    stop("Please only input dataframes that contain numeric values")
}

















