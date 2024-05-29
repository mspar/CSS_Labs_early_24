#---------------------------------#
# Title: Code for Lab7            #
# Author: Maël Lecoursonnais      #
# Initial date: 2024-05-01        #
#---------------------------------#

# This script is meant to accompany Lab7.html.
# It contains the same code, and you can use 
# this script to follow the course, and to do
# the exercises.

# Setup
Sys.setenv(LANG="en")
library(FactoMineR)
library(explor)

# For PCA:
#install.packages("FactoMineR")
#install.packages("explor")

# 1. Interpreting the output of a PCA ----

## Data set
MP <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2018/04/MediaMPs.csv")
str(MP)


## Perform a PCA in R
# Getting to know which variables are numerical
num <- sapply(MP, is.numeric)

# Do a PCA
res <- PCA(MP[, num], graph = FALSE)
res

#Plots
plot(res, choix = "ind")
plot(res, choix = "var")

## Eigenvalues, cos2, and contribution
res$eig
plot(res$eig[, 2], type = "o")

### $cos^2$
res$var$cos2

### Contribution
res$var$contrib

## `explor` and supplementary variables
explor(res)

# Now, add character variables as supplementary variables and explore the result
# with `explor`: Look at the supplementary variables, what do they tell us
# about the data?
quali <- !num
res <- PCA(MP, quali.sup = which(quali), graph = FALSE)
#explor(res)

# 2. A PCA under the hood ----
## Data set
cars
plot(cars)

### Before scaling
summary(cars)

### After scaling
carsScaled <- scale(cars)
summary(carsScaled) #After scaling
plot(carsScaled)

## Principal component intuition
plot(carsScaled, asp = T)
abline(0, 0.1, col = "red")
abline(0, 1, col = "blue")
abline(0, -2, col = "green")

#Let's figure it out!
orthoProj <- function(point, slope = 1){
    v = c(slope, 1)
    proj = ((point %*% v) / (v %*% v))
    coord = proj %*% v
    sign(proj) * sqrt(coord[1]^2 + coord[2]^2)
}

orthoRed <- data.frame(x = apply(carsScaled, 1, \(x) orthoProj(x, slope = 0.1)), y = 1.2)
orthoBlue <- data.frame(x = apply(carsScaled, 1, \(x) orthoProj(x, slope = 1)), y = 1.0)
orthoGreen <- data.frame(x = apply(carsScaled, 1, \(x) orthoProj(x, slope = -1)), y = 0.8)

plot(orthoBlue, ylab = "", yaxt = "n", col = "blue")
points(orthoRed, col = "red")
points(orthoGreen, col = "green")

round(c(Red = var(orthoRed$x), Blue = var(orthoBlue$x), Green = var(orthoGreen$x)), 3)


## Eigen decomposition
(C = cov(carsScaled))

(e <- eigen(C, TRUE))

e1 = c(e$values[1] * e$vectors[1, 1], e$values[1] * e$vectors[2, 1])
#e1 is the first eigen value multiplied by the first eigen vectors

e2 = c(e$values[2] * e$vectors[1, 2], e$values[2] * e$vectors[2, 2])
#same for e2 but for the second

#Plot
plot(carsScaled, asp = T) #asp is to get orthogonal axes
arrows(0, 0, e1[1], e1[2], col = "blue", lwd=2, length=0.1)
arrows(0, 0, e2[1], e2[2], col = "green", lwd=2, length=0.1)
abline(0, 1, col = "blue")
abline(0, -1, col = "green")

max = which.max(carsScaled[, 2])
points(x = carsScaled[max, 1], y = carsScaled[max, 2], col="red", pch=19, cex=2, asp=T)
abline(a = carsScaled[max, 1] + carsScaled[max, 2], b = -1, col = "red")
abline(a = -carsScaled[max, 1] + carsScaled[max, 2], b = 1, col = "red")

#PCs
newCoord <- as.data.frame(carsScaled %*% (-e$vectors))
colnames(newCoord) <- c("PC1", "PC2")
head(newCoord)

#Plot
plot(newCoord, asp = T)
abline(h = 0, col = "green")
abline(v = 0, col = "blue")
points(x = newCoord$PC1[max], y = newCoord$PC2[max], col = "red", pch=19, cex=2, asp=T)
abline(v = newCoord[max,1], h = newCoord[max,2], col = "red")

# 3. Your turn! ----

