#' @title Structural Attributes of Egonets Data Frame
#' 
#' @description 
#' \code{getStructureAttr} produces a data frame of the structural attributes
#'   of a list of ego networks.
#'   
#' @param egonetList A list of Igraph network objects that represent the collection
#'   of egonets.
#' 
#' @import igraph
getStructureAttr <- function(egonetList){
  
  # make sure that the argument is a list of igraph objects
  if(class(egonetList) != "list" ||
     sum(sapply(egonetList, class) == "igraph") != length(egonetList))
    stop("Please nter appropriate data as an argument.")
  
  # intialize the data frame that will contain all the information on
  # the structural characteristics of the data
  egoListLen <- length(egonetList)
  egoID <- 1:egoListLen
  df <- as.data.frame(egoID)
  
  # calculate the ego degree
  df$egoDegree <- sapply(1:egoListLen, function(x) vcount(egonetList[[x]]))
  # calculate the density
  df$density <- sapply(1:egoListLen, function(x) graph.density(egonetList[[x]]))
  # count the number of components
  df$components <- sapply(1:egoListLen, function(x) count_components(egonetList[[x]]))
  # calculate the number of edges
  df$edgeCount <- sapply(1:egoListLen, function(x) ecount(egonetList[[x]]))
  # calculate the effective size (for ego networks)
  df$effectiveSize <- df$egoDegree - (df$edgeCount * 2) / df$egoDegree
  # calculate the efficieny of the networks
  df$efficiency <- df$effectiveSize / df$egoDegree
  # calculate the mean alter degree
  df$meanAlterDegree <- (df$edgeCount * 2) / df$egoDegree
  # calculate the constraint of the graphs
  df$constraint <- sapply(1:egoListLen, function(x) sum(constraint(egonetList[[x]]), na.rm = TRUE) /
                            df$egoDegree[x])
  # calculate the hierarchy of the graphs
  df$hierarchy <- sapply(1:egoListLen, function(x) hierarchy(egonetList[[x]]))
  
  # remove the ego ID
  df$egoID <- NULL
  
  return(df)
  
}