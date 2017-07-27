# test number 2

## @knitr prepareData

library(dendextend)
library(heatmaply)
library(pvclust)

source("hegomap.R")
source("scaledColumns.R")
source("formatClusters.R")

# load the data
df.clean <- read.csv(paste("C:\\Users\\Phil\\Documents\\concordia\\",
                           "summer2017\\researchISM\\egonetDataQUALITY\\",
                           "july20thData\\egonetDataJuly20NoProp.csv", sep = ""))

# remove certain variables because they're binary or non-numeric
drops <- c("EgoID", "Sex", "whobmi4_V3", "verysedv2")
df.clean <- df.clean[, !(names(df.clean) %in% drops)]
rm(drops)

# remove egonets with missing data
df <- df.clean[complete.cases(df.clean), ]