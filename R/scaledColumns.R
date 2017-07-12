#' @title Scale Between 0 and 1
#' 
#' @description 
#' \code{scaleColumns} scales the columns of a data frame object
#'   between the values of 0 and 1 without changing the underlying
#'   distribution of the columns
#'  
#' @param df The data frame of numerical values to be scaled
scaleColumns <- function(df){
  
  scaledDf <- lapply(df, function(x) (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
  return(scaledDf)
  
}   