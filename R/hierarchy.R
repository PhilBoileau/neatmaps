#' @title Hierarchy
#' 
#' @description
#' \code{hierarchy} calculates the hierarchy of an ego network
#' 
#' @param egonet An igraph object representing an ego network
hierarchy <- function(egonet){
  
  # get the constraints of the network, their sum and the number of nodes
  cons <- constraint(egonet)
  cons[is.nan(cons)] <- 0
  sumCons <- sum(cons)
  neigh <- vcount(egonet)
  
  # calculate the vectors needed to perform the calculation
  v1 <- cons / (sumCons / neigh)
  v2 <- log(cons/ (sumCons / neigh))
  v2[is.infinite(v2)] <- 0
  h <- sum(v1 * v2) / (neigh * log(neigh))
  
  if(ecount(egonet) == 0)
    h <- 0
  
  return(h)
  
}