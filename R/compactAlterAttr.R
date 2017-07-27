#' @title Alter Attribute Compactor
#' 
#' @description 
#' \code{compactAlterAttr} creates a data frame that summarizes alter attributes.
#' 
#' @param alter.df A data frame containing all the variables that are linked to
#'  the alters in the network. If there are n networks, a maximum of x alters
#'  per network and y variables for each alter, the data frame should have n
#'  rows and x*y columns. The column names a shared attribute should be writen
#'  as follows: attr1, attr2, ... , attrx.
#' @param measureOfCent A vector that contains the measures of centrality with
#'   which to summarize the alter attributes. The supported measures are "mean"
#'   and "median". Defaults to "mean".
compactAlterAttr <- function(alter.df, measureOfCent = "mean"){
  
  # make sure that the arguments entered are of the right format
  if(class(alter.df) != "data.frame" ||
     (("mean" %in% measureOfCent) == FALSE) && ("median" %in% measureOfCent) == FALSE)
    stop("Please enter appropriate arguments.")
 
  # get all the column names in the df
  varNames <- colnames(alter.df)
  # remove all the numbers from the variable names, and remove all duplicates
  varNames <- gsub("[0-9]+$", "", varNames)
  varNames <- unique(varNames)
  
  # initialize the matrix of the summarized alter attributes
  df <- matrix(nrow = nrow(alter.df), ncol = length(varNames) * length(measureOfCent))
  
  # fill in each element of the matrix
  for(j in 1:length(varNames)){
    for(i in 1:nrow(alter.df)) {
      
      # get the vector of elements to be used in calculation
      alterData <- c()
      for(each in grep(varNames[j], colnames(alter.df)))
        alterData <- c(alterData, alter.df[i, each])
      
      # for each measure of centrality, add the data to the df
      if("mean" %in% measureOfCent && "median" %in% measureOfCent){
        df[i, j] <- mean(alterData, na.rm = TRUE)
        df[i, j + length(varNames)] <- median(alterData, na.rm = TRUE)
      } else if ("mean" %in% measureOfCent)
        df[i, j] <- mean(alterData, na.rm = TRUE)
      else
        df[i,j] <- median(alterData, na.rm = TRUE)

    }
  }
  
  # turn the df into a data frame
  df <- as.data.frame(df)
  # add the column names to the df
  if("mean" %in% measureOfCent && "median" %in% measureOfCent){
    colnames(df) <- c(sapply(1:length(varNames), function(x) paste(varNames[x], "mean", sep = ".")),
                      sapply(1:length(varNames), function(x) paste(varNames[x], "median", sep = ".")))
  } else if ("mean" %in% measureOfCent)
    colnames(df) <- sapply(1:length(varNames), function(x) paste(varNames[x], "mean", sep = "."))
  else
    colnames(df) <- sapply(1:length(varNames), function(x) paste(varNames[x], "median", sep = "."))
  
  
  return(df)
}