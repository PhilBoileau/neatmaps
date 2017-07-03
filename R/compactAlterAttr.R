#' @title Alter Attribute Compactor
#' 
#' @description 
#' \code{compactAlterAttr} creates a data frame that summarizes alter attributes
#' 
#' @param alter.df A data frame containing all the variables that are linked to
#'  the alters in the network. If there are n networks, a maximum of x alters
#'  per network and y variables for each alter, the data frame should have n
#'  rows and x*y columns. The column names a shared attribute should be writen
#'  as follows: attr1, attr2, ... , attrx.
#' @param measureOfCent A vector that contains the measures of centrality with
#'   which to summarize the alter attributes. The supported measures are "mean",
#'   and "median". Defaults to "mean".
compactAlterAttr <- function(alter.df, measureOfCent = "mean"){
  
  # make sure that the arguments entered are of the right format
  if(class(alter.df) != "data.frame" ||
     (("mean" %in% measureOfCent) == FALSE) && ("meadian" %in% measureOfCent) == FALSE)
    stop("Please enter appropriate arguments.")
  
}