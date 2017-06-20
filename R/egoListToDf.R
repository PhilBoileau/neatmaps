#' @title Turn list of Egonets Into Dataframe
#'
#' @description
#' \code{egoListToDataframe} creates a dataframe of graph attributes
#'
#' @details
#' This functions takes a list of ego networks and produces a data frame of
#' of the graph attributes and of the measures of central tendencies of each
#' networks' vertex attributes. Only numeric attributes are considered.
#' Additionnaly, measures of homophily can be computed for the specified
#' variables.
#' 
#' @param data the list of ego networks (must be igraph objects)
#' @param centralMeasures a vector of the required measures of central
#'   tendicies. The supported measures are: mean, mode and median.
#' @param homoVars a vector of variables which require measures
#'   of homophily to be calculated. The supported measures are:
#'   homophily, EIindex, IQV
#' @param homoMeasures a vector of the various homophily measurements
#'   to be calculated for each of the variables given in homoVars
#' @method create a dataframe of graph attributes
#' @export
#' @return a data frame of graph attributes for each ego network
#' @author Phil Boileau <philippe.boileau@mail.concordia.ca>
#' @example
#' ego.df <- egoListToDataframe(data = egoList,
#'                               centralMeasures = c("mean", "median"),
#'                               homoVars = c("gender"),
#'                               homoMeasures = c("EIindex", "homophily"))
egoListToDataframe <- function(data, centralMeasures, homoVars, homoMeasures){
  
  # avoid arguments that would cause errors
  if(class(data) != "list" || class(data[[1]]) != "igraph")
    stop("data must be a list of igraph objects")
  
  
  # call the homophily measure calculator function
  # calculateHomo(data = data, homoVars = homoVars, homoMeasures = homoMeasures)
  
  
  # call the vertex attribute calculator function
  # calculateVertAttr(data = data, centralMeasures = centralMeasures)
  
  
  # create the dataframe using the data
  packedDf <- as.data.frame(t(sapply(1:length(data), function(x) graph_attr(data[[x]]))))
  attrNames <- colnames(packedDf)
  attrDf <- as.data.frame(sapply(1:length(packedDf), function(x) unlist(packedDf[, x])))
  colnames(attrDf) <- attrNames
  
  return(attrDf)
  
}



















