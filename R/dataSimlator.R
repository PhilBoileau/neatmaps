#' @title Simulate ego network data
#'
#' @description
#' \code{simulateEgonets} simulates ego networks and their attributes.
#'
#' @details
#' This function generates a list of ego networks and related attributes.
#' These networks and attributes are generated randomly, and therefore hold no
#' real value. This function should only be used to produce data when none other
#' is available. The networks are created using the random graph model. the number of
#' vertices per egonet is normally distributed with mean m and standard deviation s.
#' Fifteen graph attributes are randomly generated, and a handful of structural
#' characterisitcs are calculated.
#'
#' @param n number of egonets to simulate (set to 50 if not mentionned)
#' @param m mean number of alters per egonet (set to 10 if not mentionned)
#' @param s standard deviations of alters per egonet (set to 3 if not mentionned)
#' @param p probability of an edge between vertices for GNP model (set to 0.2 if not mentionned)
#' @method simulate egonet data
#' @export
#' @return array of n \url{http://igraph.org/} objects
#' @author Phil Boileau <philippe.boileau@mail.concordia.ca>
#' @example
#' egoNetList <- simulateEgonets(n = 50, m = 7, s = 2)
simulateEgonets <- function(n = 50, m = 10, s = 3, p = 0.2){

  # avoid arguments that would cause errors
  if(n < 1 || n%%1 != 0 || is.numeric(n) == FALSE)
    stop("n must be an integer greater than or equal to 1")
  if(m < 1 || is.numeric(m) == FALSE)
    stop("m must be a number greater than 1")
  if(s < 0 || is.numeric(s) == FALSE)
    stop("s must be a positive number")
  if(p < 0 || p > 1 || is.numeric(p) == FALSE)
    stop("p must be a number between 0 and 1")

  # generate the number of alters per egonet. This value cannot be less than one
  egoDegree <- round(rnorm(n, mean = m, sd = s))
  egoDegree[which(egoDegree < 1)] <- 1

  # create the vector of n egonets
  egoNets <- sapply(1:n, function(x) simplify(erdos.renyi.game(egoDegree[x], p, type = "gnp", directed = F)))

}






