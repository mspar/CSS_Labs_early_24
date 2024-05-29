#---------------------------------#
# Title: Code answers for Lab10   #
# Author: Maël Lecoursonnais      #
#         & Etienne Ollion        #
# Initial date: 2024-05-16        #
#---------------------------------#

# This script includes the code to answer 
# the exercises 1, 2 of the Lab10

# Setup ----
Sys.setenv(LANG="en")
library(Rtsne)
library(aweSOM)
library(Hmisc)
library(ggplot2)

# t-SNE ----
dat <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2019/05/ForeignBorninSwedenbyMunicipality2012.csv")

#Data wrangling
dat[is.na(dat)] <- 0
rownames(dat) <- dat$Kommun
dat <- dat[, -c(1:2)]

#Let's create 3 datasets
dat_wout_Sweden <- subset(dat, select = -Sweden)
dat_wout_bigcities <- subset(dat_wout_Sweden, !rownames(dat_wout_Sweden) %in% c("Stockholm", "Göteborg", "Malmö"))

#1. 
set.seed(12345)
data.set <- scale(dat)
sne <- Rtsne(data.set)
str(sne)
plot(sne$Y)

#2. 
set.seed(12345)
data.set2 <- scale(dat_wout_bigcities)
sne2 <- Rtsne(data.set2)
str(sne2)
plot(sne2$Y)

FactoMineR::PCA(data.set2)

#3.
#By volume of foreign born
group <- cut2(rowSums(dat, na.rm = TRUE), g = 4)
table(group)
plot(sne$Y, col = group)

#By number of 'nationalities' represented
nb <- rowSums(dat != 0)
ggplot() +
  aes(x = sne$Y[, 1], y = sne$Y[, 2], color = nb) +
  geom_point() +
  scale_color_distiller("#", palette = "Spectral") +
  theme_bw()


# SOM ----
aweSOM()
