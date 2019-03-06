#' @title Consensus Cluster Plus without Plots
#' 
#' @description 
#'   \code{consensusClusterNoPlots} is a wrapper function for
#'   \link{ConsensusClusterPlus} that suppressess the the creation of the
#'   plots that are created automatically.
#' 
#'  @param ... These paramaters are passed directly to 
#'  \link{ConsensusClusterPlus}. See the \link{ConsensusClusterPlus} manual for
#'  more details. 
#'  
#'  @importFrom ConsensusClusterPlus ConsensusClusterPlus
consensusClusterNoPlots <- function(...){
  ff <- tempfile()
  png(filename=ff)
  res <- ConsensusClusterPlus(...)
  dev.off()
  unlink(ff)
  res
}