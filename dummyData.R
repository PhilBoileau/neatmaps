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
# make sure none are zero
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


# create a dataframe to store info on all the simulated values
# add bmi
# generate the values for the BMI
bmi <- rnorm(40, mean = 25, 5)
bmiAve <- rnorm(40, mean = 25, 2)
overview.df <- as.data.frame(bmi)
overview.df$bmiAve <- bmiAve
rm(bmi, bmiAve)

# add ego degree
overview.df$egoDeg <- vertNum

# add gender (0 is girls, 1 is boys)
overview.df$gender <- round(runif(40))

# calculate the values of (insert here) for each network
overview.df$homo <- runif(40)
overview.df$krackhardt <- runif(40, min = -1, max = 1)
overview.df$yulesq <- runif(40, min = -1, max = 1)
overview.df$IQV <- runif(40)
overview.df$constrain <- runif(40)
overview.df$hierarchy <- runif(40)

# further calculations of the networks structural properties
edgeDensity <- c()
components <- c()
edgeCount <- c()
effectiveSize <- c()
efficiency <- c()

for(i in 1:40){
  
  edgeDensity <- c(edgeDensity, edge_density(egoNets[[i]], loops = FALSE))
  components <- c(components, count_components(egoNets[[i]]))
  edgeCount <- c(edgeCount, gsize(egoNets[[i]]))
  effectiveSize <- c(effectiveSize, vcount(egoNets[[i]]) - sum(degree(egoNets[[i]], mode = "all"))/vertNum[i])
  efficiency <- c(efficiency, effectiveSize[i] / vertNum[i])

}

# add values to the df
overview.df$density <- edgeDensity
overview.df$components <- components
overview.df$edgeNum <- edgeCount
overview.df$effectiveSize <- effectiveSize
overview.df$efficiency <- efficiency

rm(i, edgeDensity, vertNum, components, edgeCount, effectiveSize, efficiency)
par(mfrow=c(1, 1))



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
    relativeBmiMatrix[i, j] <- (ref - bmi[i]) / mean(ref, bmi[i])
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
    relativeDegMatrix[i, j] <- (ref - egoDeg[i]) / mean(ref, egoDeg[i])
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

