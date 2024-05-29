#---------------------------------#
# Title: Code for Lab9            #
# Author: Maël Lecoursonnais      #
# Initial date: 2024-05-13        #
#---------------------------------#

# This script is meant to accompany Lab9.html.
# It contains the same code, and you can use 
# this script to follow the course, and to do
# the exercises.

# Setup
Sys.setenv(LANG="en")
library(FactoMineR)
library(explor)
library(NbClust)
library(factoextra)
library(cluster)

#' # For clustering
#' install.packages("cluster")
#' install.packages("NbClust")
#' install.packages("factoextra")
#' install.packages("ade4")
#' 
#' # For PCA:
#' install.packages("FactoMineR")
#' install.packages("explor")

#' # 1. Running example

set.seed(12345)

mickeyFunction <- function(n1, n2){
  head <- data.frame(x= runif(n1, min= -1), y= runif(n1, min= -1))
  head <- head[rowSums(head^2) <= 1, ]
  
  left_ear <- data.frame(x= runif(n2, min= -1), y= runif(n2, min= -1))
  left_ear <- left_ear[rowSums(left_ear^2) <= 1, ]
  left_ear$x <- left_ear$x / 2 - 1.05
  left_ear$y <- left_ear$y / 2 + 1.05
  
  right_ear <- data.frame(x= runif(n2, min= -1), y= runif(n2, min= -1))
  right_ear <- right_ear[rowSums(right_ear^2) <= 1, ]
  right_ear$x <- right_ear$x / 2 + 1.05
  right_ear$y <- right_ear$y / 2 + 1.05
  
  mickey <- rbind(head, left_ear, right_ear)
  return(scale(mickey))
}

mickey <- mickeyFunction(300, 100)

plot(mickey, main = "How would you cluster me?", asp = T)
grid()

#' # 2. Partitioning methods

#' ## k-means

animation::ani.options(interval = 2)

#' ### 2 clusters
animation::kmeans.ani(mickey, 2)
km.res <- kmeans(mickey, 2, nstart = 10)
plot(mickey, main = "Kmeans-2", col = km.res$cluster, asp = T)

#' ### 3 clusters
animation::kmeans.ani(mickey, 3)
km.res <- kmeans(mickey, 3, nstart = 10) 
plot(mickey, main = "Kmeans-3", col = km.res$cluster, asp = T)

#' ### 4 clusters
km.res <- kmeans(mickey, 4)
plot(mickey, main = "Kmeans-4", col = km.res$cluster, asp = T)

#' ## k-medoids

#' ### 2 clusters
pam.res <- pam(mickey, 2, metric = "euclidean")
plot(mickey, main = "PAM-2", col = pam.res$cluster, asp = T)

#' ### 3 clusters
pam.res <- pam(mickey, 3, metric = "euclidean") 
plot(mickey, main = "PAM-3", col = pam.res$cluster, asp = T) 

#' ## Determining the number of clusters 
#' ### Within-cluster sum of squares
fviz_nbclust(mickey, kmeans, method = "wss")

#' ### Silhouette
fviz_nbclust(mickey, kmeans, method = "silhouette")

#' ### Gap
fviz_nbclust(mickey, kmeans, method = "gap_stat", k.max = 5, nboot = 10)

#' # 3. Hierarchical Clustering
#' ## Running example
#' 
#' Let's get back to Mickey. Create a distance matrix, and then create a
#' clustering based on a single linkage.
d <- dist(mickey, method = "euclidean")
hc.res <- hclust(d = d, method = "single")

#' We can now plot the dendrogram and decide where to cut it
plot(hc.res, cex = 0.5, hang = -1)
nb <- cutree(hc.res, k = 2)
rect.hclust(hc.res, k = 2, border = 2:5)

#' Plot mickey with the two clusters.
plot(mickey, main = "Single", col = 5, asp = T)

#' ## Choosing the number of clusters {.tabset}
#' ### Single - silhouette
fviz_nbclust(scale(mickey), hcut, hc_method = "single", method = c("silhouette"))

#' ### Complete - silhouette
fviz_nbclust(scale(mickey), hcut, hc_method = "complete", method = c("silhouette"))

#' ### Complete - wss
fviz_nbclust(scale(mickey), hcut, hc_method = "complete", method = c("wss"))

#' ### NbClust
## nbclust <- NbClust(data = scale(mickey), distance = "euclidean", min.nc = 2, max.nc = 5, method = "ward.D2")

#' ## Validation with external variables
pam.iris <- pam(scale(iris[, 1:4]), 3)
table(iris$Species, pam.iris$clustering)

# 4. Exercise 1 - HC on Mickey

# 5. Exercise 2 - Valkompass

# 6. (Optional) Exercise 3 - Swedish municipalities


