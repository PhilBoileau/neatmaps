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
#' @param scale choice of scalling method to use on the data. The choices
#'   are: "none", "normalize" and "reduce". "none" does not
#'   apply scaling to the dataframe. "normalize" assumes that all variables
#'   are normally distributed and transforms the data into the standard
#'   normal distribution. "reduce" transforms all data to be between the
#'   value of 0 and 1, all while maintaining the shape of their original
#'   distribution.
#' @param printCluster a boolean value to determine if the cluster analysis
#'   should be printed to the console.
#' @export
#' @return an interactive heatmap, as well as a hierarchal clustering 
#'   analysis on the ego networks and on their attributes
#' @author Phil Boileau <philippe.boileau@mail.concordia.ca>
#' @example
#' 
hegomaps <- function(df, scale = "reduce", printCluster = TRUE){
  
  # check dataframe to make sure that it only contains numeric values
  if(FALSE %in% sapply(df, is.numeric))
    stop("Please only input dataframes that contain numeric values")
}

















