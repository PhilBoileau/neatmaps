# In this file, I will generate 40 ego networks with a small number of characteristics.

# The number of alters in each network will be generated at random based on a normal distribtuion with
# mean 7 and with standard deviation 3.

# the bmi of the egos will be randomly generated using a normal distribution with mean 25, std of 5

# the rest of the structural values will be calculated using the igraph library

# --------------------------------------------------------------------------------------------------------------

# import igraph
library(igraph)

# create a vector of 40 vertices that follow a normal distribution with mean 7 and std 3
# essentially the degree of the ego
vertNum <- ceiling(rnorm(40, mean = 7, sd = 3))
# make sure none are zero or negative
vertNum[which(vertNum <= 0)] <- 1

# create a list of 40 ego-networks
egoNets <- list()
for(i in 1:40){

  erdosI <- simplify(erdos.renyi.game(vertNum[i], 0.2, type = "gnp", directed = F))
  egoNets <- append(egoNets, list(erdosI))

}


# for the fun of it, graph the first nine ego Networks
par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
for(j in 1:9){

  plot(egoNets[[j]], edge.arrow.size = .2, vertex.color = "blue", vertex.label = NA,
       edge.arrow.mode = 0, layout = layout_in_circle(egoNets[[j]]), main = j)

}

# clean up
rm(i, j, erdosI)
#dev.off()


# add all the graph attributes to the list of egonets
# generate the values for the BMI
bmi <- rnorm(40, mean = 25, 5)
bmiAve <- rnorm(40, mean = 25, 2)
for(i in 1:length(egoNets)){

  egoNets[[i]]$bmi <- bmi[i]
  egoNets[[i]]$bmiAve <- bmiAve[i]

}
rm(bmi, bmiAve)

# add ego degree
for(i in 1:length(egoNets)){

  egoNets[[i]]$egoDegree <- vertNum[i]

}

# add gender (0 is girls, 1 is boys)
for(i in 1:length(egoNets)){

  egoNets[[i]]$gender <- as.numeric(runif(1) > 0.5)

}

# calculate the values of (insert here) for each network
for(i in 1: length(egoNets)){

  egoNets[[i]]$homo <- runif(1)
  egoNets[[i]]$krackhardt <- runif(1, min = -1, max = 1)
  egoNets[[i]]$yulesq <- runif(1, min = -1, max = 1)
  egoNets[[i]]$IQV <- runif(1)
  egoNets[[i]]$constrain <- runif(1)
  egoNets[[i]]$hierarchy <- runif(1)

}

# further calculations of the networks structural properties
for(i in 1:length(egoNets)){

  egoNets[[i]]$edgeDensity <- edge_density(egoNets[[i]], loops = FALSE)
  egoNets[[i]]$components <- count_components(egoNets[[i]])
  egoNets[[i]]$edgeCount <- gsize(egoNets[[i]])
  egoNets[[i]]$effectiveSize <- egoNets[[i]]$egoDegree -
                                  sum(degree(egoNets[[i]]))/egoNets[[i]]$egoDegree
  egoNets[[i]]$efficiency <- egoNets[[i]]$effectiveSize / egoNets[[i]]$egoDegree

}

rm(i, vertNum)
# reset the number of plots per frame
par(mfrow=c(1, 1))

# create a dataframe based on the graph attribute of each egonet
attrList <- as.data.frame(t(sapply(1:length(egoNets), function(x) graph_attr(egoNets[[x]]))))

# drop some of the attributes that we are not interested in:
attrList <- attrList[-seq(1:4)]

# unlist all of the columns of the matrix, name the columns
overview.df <- as.data.frame(sapply(1:length(attrList), function(x) unlist(attrList[, x])))
colnames(overview.df) <- colnames(attrList)
rm(attrList)


# order the data based on BMI to get a clear plot
# overview.df <- overview.df[order(overview.df$bmi), ]


# time to create the first heatmap.
# This will just give an overview of the various characteristics of each egonet
library(d3heatmap)
d3heatmap(overview.df, scale = "column", dendrogram = "none", colors = "YlOrRd", main = "Networks Overview")
# although i really like this package, it is not actively maintained


library(heatmaply)
heatmaply(percentize(overview.df),
          dendrogram = "none",
          main = "Overview of Egonets",
          xlab = "Characteristics",
          ylab = "Egonets",
          margins = c(50, 50, 50, 100),
          colors = heat.colors(100),
          cexRow = 6,
          cexCol = 6,
          cellnote = overview.df,
          cellnote_color = "#000000",
          draw_cellnote = F)

# this seems pretty solid... I'm going to email sahir to see if it would be possible to
# alter the infowindow to show the actual value, and not the normalized value


# the next plot that I want to create is a plot of relative differences between
# each of the egonets for a single property of the networks. This can ego centric,
# alter centric or structural. We will let relative difference be defined as
# (x - y) / mean(x, y) where x is the reference point

# lets start off with bmi
bmi <- overview.df$bmi
relativeBmiMatrix <- matrix(data = NA, nrow = 40, ncol = 40)

# loop through each entry, going column by column
for(j in 1:40){
  # set the reference bmi
  ref <- bmi[j]

  for(i in 1:40) {
    # perform the calculation and update the matrix
    relativeBmiMatrix[i, j] <- (ref - bmi[i]) / mean(c(ref, bmi[i]))
  }
}

# plot the heatmap
heatmaply(relativeBmiMatrix,
          dendrogram = "none",
          main = "Relative BMI",
          xlab = "Egonets (Reference)",
          ylab = "Egonets (Compared)",
          cexRow = 6,
          cexCol = 6,
          margins = c(50, 50, 50, 50))


# try using a structural value, such as ego degrees

egoDeg <- overview.df$egoDeg
relativeDegMatrix <- matrix(ncol = 40, nrow = 40)

# loop through each entry, going column by column
for(j in 1:40){
  # set the reference bmi
  ref <- egoDeg[j]

  for(i in 1:40) {
    # perform the calculation and update the matrix
    relativeDegMatrix[i, j] <- (ref - egoDeg[i]) / mean(c(ref, egoDeg[i]))
  }
}

# plot the heatmap
heatmaply(relativeDegMatrix,
          dendrogram = "none",
          main = "Relative Ego Degree",
          xlab = "Egonets (Reference)",
          ylab = "Egonets (Compared)",
          cexRow = 6,
          cexCol = 6,
          margins = c(50, 50, 50, 50))

# do the exact same thing but for relative difference in edge numbers

edgeNum <- overview.df$edgeCount
relativeEdgMatrix <- matrix(ncol = 40, nrow = 40)

# loop through each entry, going column by column
for(j in 1:40){
  # set the reference bmi
  ref <- edgeNum[j]

  for(i in 1:40) {
    # perform the calculation and update the matrix
    diffe <- (ref - edgeNum[i]) / mean(c(ref, edgeNum[i]))
    if (is.infinite(diffe) || is.nan(diffe))
      relativeEdgMatrix[i, j] <- 0
    else
      relativeEdgMatrix[i, j] <- diffe
  }
}

# plot the heatmap
heatmaply(relativeEdgMatrix,
          dendrogram = "none",
          main = "Relative Number of Edges",
          xlab = "Egonets (Reference)",
          ylab = "Egonets (Compared)",
          cexRow = 6,
          cexCol = 6,
          margins = c(50, 50, 50, 50))

