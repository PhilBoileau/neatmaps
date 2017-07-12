#' @title Ego Network Data Frames
#' 
#' @description 
#' \code{egonet.data.frame} produces data frames of collections of ego networks.
#' 
#' @details
#' The function produces data frames of collections of ego networks. The function
#' requires the input of three data frames: a data frame containing the graph
#' attributes, a data frame containing the alter characteristics and a data frame
#' containing the edgelist of each network. The rows in each of these data frames
#' must represent individual ego networks, and must therefore have identical row
#' length. Measures of centrality used in the summarization of the alter attributes
#' must also be furnished. If users wish to analyze the homophily of alters in the 
#' ego networks, the variables to be analyzed must be indicated along with the
#' measures of homophily to be used. All of the ego networks should be of similar
#' size.
#' 
#' @param net.attr.df A data frame consisting of all of the ego networks' graph
#'   attributes. The first column should contain the name of the network, and all
#'   other columns should be numeric. All empty entries should be filled as "NA".
#' @param alter.attr.df A data frame consisting of all of the ego networks' alters'
#'   attributes. All columns should be numeric. All empty entries should be filled
#'   in as "NA".
#' @param edge.df A dataframe consisting of the edge matrix for each ego network.
#'   Edges are assumed to be undirected and unweighted. 1 indicates the existence 
#'   of an edge between alters, 0 indicates the lack of an edge.
#' @param measuresOfCent A vector of the measures of centrality to be used for the
#'   summary of the alter attributes data. The supported measures of centrality
#'   are: "mean", "mode" and "median". Defaults to "mean".
#' @param homoVars A vector consisting of the name of the variables on which homophily
#'   is to be measured. These variables are dropped from the final data frame since 
#'   they are catigorical variables.
#' @param homoMeasures A vector consisting of the various methods to calculate homophily
#'   with. The supported measures are: "homophily", "EIindex" and "IQV".
#' @export
#' @author Phil Boileau <philippe.boileau@mail.concordia.ca>  
#' @return The function returns a data frame that offers an overview of all of the
#'   ego networks.
#' 
#' 
#' @examples
#' 
egonet.data.frame <- function(net.attr.df, alter.attr.df, edge.df,
                              measuresOfCent = c("mean"), scale.df = "none"){

  # Make sure that only data frames are being passed in to the three first arguments
  if(class(net.attr.df) != "data.frame" || class(alter.attr.df) != "data.frame" ||
     class(edge.df) != "data.frame" || nrow(net.attr.df) != nrow(alter.attr.df) ||
     nrow(net.attr.df) != nrow(edge.df))
    stop("Please make sure tha net.attr.df, alter.attr.df and edge.df 
         are dataframes of the same length.")
  
  
  # define the data frame to be returned, set initially to net.attr.df
  df <- net.attr.df
  
  
  # create the igraph network list of all the egonets, only including variables used
  # to measure homophily
  egonetList <- createNetworks(edge.df)
  
  # get the structural characteristics of each of egonetworks
  structure.df <- getStructureAttr(egonetList)
  
  
  # drop the homophily measure variables from the alter.attr.df
  # alter.attr.df <- alter.attr.df[, !(names(alter.attr.df) %in% homoVars)]
  
  # get the measures of centrality of the alter characteristics
  alter.measures.df <- compactAlterAttr(alter.attr.df, measuresOfCent)
  
  
  # combine all three data frames to get the final df
  df <- cbind(df, structure.df, alter.measures.df)
  
  
  # return the final product
  return(df)
  
}











