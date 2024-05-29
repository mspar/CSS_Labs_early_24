#---------------------------------#
# Title: Code for Lab10           #
# Author: Maël Lecoursonnais      #
#         & Etienne Ollion        #
# Initial date: 2024-05-16        #
#---------------------------------#

# This script is meant to accompany Lab10.html.
# It contains the same code, and you can use 
# this script to follow the course, and to do
# the exercises.

MPs <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2018/04/MediaMPs.csv")

#### t-SNE ----
#Libraries
library(Rtsne)
library(ggplot2)

#Data set
MPs <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2018/04/MediaMPs.csv")

#Select active variables only
dat_tsne <- scale(MPs[, 2:8])
sne <- Rtsne(dat_tsne)

set.seed(12345)
sne <- Rtsne(jitter(dat_tsne))

#plot it
plot(sne$Y)

#Explore the structure

#Let's delve into this dataset this
##Color by gender
color_map <- c(`F` = "darkblue", M = "darkred")

plot(sne$Y, col = color_map[MPs$Gender])
legend("bottomleft", col= color_map, pch= 16, legend = levels(MPs$Gender))

##Color by party
colors <- rainbow(length(unique(MPs$Party))) 
color_map <- setNames(colors, unique(MPs$Party))

plot(sne$Y, col= color_map[MPs$Party])
legend("bottomleft", col = color_map, pch= 16, legend = unique(MPs$Party))

##Color by frequency of invitation
MPs$group <- Hmisc::cut2(rowSums(MPs[, 2:8]), g = 6)
# group
#       0       1       2 [ 3,10) [10,86] 
#     260      89      39      99      87

colors <- topo.colors(length(levels(MPs$group))) 
color_map <- setNames(colors, levels(MPs$group))

plot(sne$Y, col= color_map[MPs$group])
legend("bottomleft", col= color_map, pch= 16, legend = levels(group)) 

#SOMS -------

#install.packages("aweSOM")
library(aweSOM)
write.csv(MPs, "MediaMPs.csv")
aweSOM()

#Import the data set
#Select the parameters (for ex., a 5x5 rectangular map, initialized by PCA)
#Plot it, add colours according to gender, party
#Add superclasses. How many? Why?

