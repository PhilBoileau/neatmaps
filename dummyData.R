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
vertNum[which(vertNum == 0)] <- 1

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
rm(i, j)
dev.off()



# create a dataframe to store info on all the simulated values
overview.df <- data.frame()

# add bmi
# generate the values for the BMI
bmi <- rnorm(40, mean = 25, 5)
bmiAve <- rnorm(40, mean = 25, 2)
overview.df$bmi <- bmi
overview.df$bmiAve <- bmiAve
rm(bmi, bmiAve)

# add ego degree
overview.df$egoDeg <- vertNum

# calculate the values of (insert here) for each network
overview.df$homo <- runif(40)
overview.df$krackhardt <- runif(40, min = -1, max = 1)
overview.df$yulesq <- runif(40, min = -1, max = 1)
overview.df$IQV <- runif(40)




















