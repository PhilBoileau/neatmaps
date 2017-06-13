# d3heatmap package
# trying it out to see if this is what I will use for to build our package

install.packages('d3heatmap')

# heres an example
library(d3heatmap)
url <- "http://datasets.flowingdata.com/ppg2008.csv"
nbaPlayers <- read.csv(url, row.names = 1)
d3heatmap(nbaPlayers, scale = "column", dendrogram = "none", color = "Blues")
