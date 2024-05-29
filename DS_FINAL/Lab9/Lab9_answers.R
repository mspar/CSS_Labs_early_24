#---------------------------------#
# Title: Code answers for Lab9    #
# Author: Maël Lecoursonnais      #
# Initial date: 2024-05-13        #
#---------------------------------#

# This script includes the code to answer 
# the exercises 1, 2 and 3 of the Lab9

# Setup ----
Sys.setenv(LANG="en")
library(FactoMineR)
library(explor)
library(NbClust)
library(factoextra)
library(cluster)

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

mickey <- mickeyFunction(1000, 400)

# Exercise 1 - HC on Mickey ----

#1. Ward and complete
#Ward
d <- dist(mickey, method = "euclidean")
hc.res <- hclust(d = d, method = "ward.D2")
plot(hc.res, cex = 0.5, hang = -1)

nb <- cutree(hc.res, 3)
rect.hclust(hc.res, k = 3, border = 2:5)

plot(mickey, main = "Ward", col = nb, asp = T)

#Complete
hc.res <- hclust(d = d, method = "complete")
plot(hc.res, cex = 0.5, hang = -1)

nb <- cutree(hc.res, 3)
rect.hclust(hc.res, k = 3, border = 2:5)

plot(mickey, main = "Complete", col = nb, asp = T)

#2. Evaluate the quality via silhouette:

#Change hc_method to "ward" for the Ward.D2 distance.
hcut.res <- hcut(d, k = 3, hc_func = "hclust", hc_method = "complete", hc_metric = "euclidean")
fviz_silhouette(hcut.res, palette = "jco", ggtheme = theme_classic(), title = "PAM-Mickey")

# 3. 
set.seed(12345)
mickey <- mickeyFunction(3000, 1000)

d <- dist(mickey, method = "euclidean")
hc.res <- hclust(d = d, method = "ward.D2")
plot(mickey, main = "Ward", col = cutree(hc.res, 3), asp = T)

# Adding more points makes the cluster more distinguishable, 
# and both PAM and HC manages to retrieve the original cluster quite well.

# Exercise 2 - Valkompass ----
dat <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2023/05/Valkompass2014_ShortRecoded_anon.csv", encoding = "UTF-8")

#1.
#Access the variables about questions (start with Q)
active <- grep("^Q", colnames(dat), value = TRUE)
#Disjunctive table
datCluster <- ade4::acm.disjonctif(dat[, active])
#Full data with one-hot encoded questions
datCluster_full <- cbind(dat[, setdiff(colnames(dat), active)], datCluster)

#2. Perform a $k$-means clustering. How many clusters would you choose? 
#   Plot the result with `fviz_cluster`, and try to assess the optimal 
#   number of cluster with `fviz_nbclust.`

## Kmeans

#With k-means, 2 is the optimal number of clusters (with the silhouette method)
fviz_nbclust(datCluster, kmeans, method = "silhouette")

#Try with 2 to 5 groups
#Note that the plot should be over all variables (n-dimensional). But here, 
#fviz_cluster plots it on the two first PCs of a PCA applied to the data set.

km.res <- kmeans(datCluster, 3, nstart = 10)
fviz_cluster(km.res, data = datCluster, ggtheme = theme_classic())

#3. Do the same but with PAM clustering

##PAM
fviz_nbclust(datCluster, pam, method = "silhouette")
pam.res <- pam(datCluster, 2)
fviz_cluster(pam.res, data = datCluster, ggtheme = theme_classic()) 
fviz_silhouette(pam.res, palette = "jco", ggtheme = theme_classic(), title = "PAM-SwedishMPs")

#4. Repeat the analysis with hierarchical clustering 

##Hierarchical clustering
hc <- dist(datCluster, method = "euclidean") #Distance
res.hc <- hclust(hc, method = "ward.D2") #Linkage
fviz_nbclust(datCluster, hcut, hc_method = "ward.D2", method = c("silhouette"))

plot(res.hc, cex = 0.5, hang = -1)
rect.hclust(res.hc, k = 3)

#5. Look at the crosstable between individuals' political party and their
#cluster:
cluster <- cutree(res.hc, 2)
table(datCluster_full$PoliticalParty, cluster)

cluster <- cutree(res.hc, 5)
table(datCluster_full$PoliticalParty, cluster)

# Political party is very highly correlated with the cluster composition.

#6. What if we used pre-processed data, e.g. the first 2 PCs of a
#PCA?

res.pca <- PCA(datCluster)
newDatCluster <- res.pca$ind$coord[, 1:2]

hc <- dist(newDatCluster, method = "euclidean") #Distance
res.hc <- hclust(hc, method = "ward.D2") #Linkage
plot(res.hc, cex = 0.5, hang = -1)
rect.hclust(res.hc, k = 3, border = 2:7)
fviz_nbclust(datCluster, hcut, hc_method = "ward.D2", method = c("silhouette"))

cluster <- cutree(res.hc, 3)
plot(newDatCluster[, 1:2], col = cluster)

# (Optional) Exercise 3 - Swedish municipalities ----

# Data:
# Swedish Municipalities
df <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2019/05/ForeignBorninSwedenbyMunicipality2012.csv",encoding = "UTF-8")
rownames(df) <- df$Kommun
df <- df[, -c(1:2)]
df[is.na(df)] <- 0

df1 <- df[,-which(colnames(df) %in% c("Sweden", "Finland", "Norway"))]
df2 <- df1[-which(rownames(df1) %in% c("Malmö", "Göteborg", "Stockholm")),]

## Kmeans
km.res <- kmeans(df2, 2)
table(km.res$cluster)
fviz_cluster(km.res, data = df2, ggtheme=theme_classic())

##PAM
pam.res <- pam(df, 3)
table(pam.res$clustering)
fviz_cluster(pam.res, data = df, ggtheme=theme_classic())
fviz_silhouette(pam.res, palette = "jco",
                ggtheme = theme_classic(), title="PAM-SwedishCities")

##Hierarchical clustering
hc <- dist(df2, method = "euclidean")
res.hc <- hclust(hc, method = "ward.D2")
fviz_dend(res.hc, k = 3)

res.hc.nbclust <- NbClust(
  data = df2,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "ward.D2" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)

