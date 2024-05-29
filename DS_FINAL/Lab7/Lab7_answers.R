#---------------------------------#
# Title: Code answers for Lab7    #
# Author: Maël Lecoursonnais      #
# Initial date: 2024-05-01        #
#---------------------------------#

# This script includes the code to answer 
# the exercise of Lab7, and a complete analysis of a PCA output.

# Note: these answers are example of what a very good analysis looks like. 
# You can take example from that for your final essay. 
# The analysis might be a bit lengthy, but this is also to show you we can
# say a lot with a PCA. Also, here, the individuals plot (the rows) are as
# informative as the columns. Note that this is not always the case.

# Setup ----
Sys.setenv(LANG="en")
library(FactoMineR)
library(explor)

# Exercise 1. ----

#1.
dat <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2019/05/ForeignBorninSwedenbyMunicipality2012.csv", encoding = "UTF-8")
rownames(dat) <- dat$Kommun
dat <- dat[, -c(1:2)] 
dat[is.na(dat)] <- 0

#2. Possible functions to use:
nrow(dat)
ncol(dat)
dim(dat)
collapse::descr(dat)
summary(dat)

# Possible answer: The data set consists of 290 rows and 53 columns. 
# The rows indicate municipalities while columns indicate country of 
# origin. All columns are numerical and cells indicate the raw number of 
# individuals for a given municipality and born in a given country or region.

#3. Do the PCA, and explore
res <- PCA(dat)
explor::explor(res)

# Possible answer: The PCA returns a highly skewed eigenvalues distribution,
#with the first PC explaining 83.4% of the variance, the second one 8.9%,
#and the others less than 2% each. Given that the two first PCs explain 92%
#of the total variance, I choose to focus my analysis on the two first PCs.
#
# The fact that the first principal component explains most of the variance
#indicates that the data set is highly correlated. We can think about it in 
#two ways: on the one hand, it suggests that municipalities have similar
#demographic structures (correlation across rows), on the other, it suggests
#that migration groups are highly polarized. A visual investigation of the 
#PCs should inform us more about these links.
#
# The variables plot of the first two PCs shows that all have positive values 
#on the first axis, meaning that the number of individuals in any country of 
#birth is highly correlated with the others. In other words, if a 
#municipality contains a lot of foreign-born from one group, it is very 
#likely that it also contains a lot of foreign-born individuals from other 
#groups. Looking at the contribution metrics, we see that most variables are 
#contributing to this PC almost equally (around 2.00), with the notable 
#exception of a few groups, including Denmark and "Former Yugoslavia 
#..Albania", which might indicate that these groups have additional 
#geographic patterns of settlement than the ones captured in the first PC.
#
# The individuals plot displays further insights to the first PC. The most
#contributing municipalities are somewhat ordered by their number of
#inhabitants, Stockholm being first with a contribution of 61%, then Malmö 
#(16%) and Göteborg (10%). Other cities have marginal contributions. This
#indicates that the "population effect" we see in the first PC is primarily
#driven by the big cities, which attracts most of the immigrants. 
#
# There is more diversity in coordinates in the second PC. Some, like "Denmark"
#or "Yugoslavia" have positive coordinates, others, like "Eritrea", have
#negative ones. The most contributing groups have positive coordinates,
#including the one mentioned above, other Balkan countries, as well as Lebanon
#and Vietnam. Only one group from the top 10 most contributing groups has
#negative coordinates: Eritrea. This supports the idea that the contributing
#groups with positive coordinates have a distinct, additional settlement
#pattern. We can for instance suppose that Danes, besides being concentrated in
#big cities like other foreign-born, are over represented in Skåne, a region
#that is limitroph to Denmark. 
#
#Looking at the individuals plot, representing municipalities, we indeed see
#that the second axis exhibits specific demographic patterns, as all of the
#most contributing municipalities are located in the South-West of Sweden,
#including a majority of Skåne cities like Malmö, Helsingborg, or Landskrona,
#but also from other Western regions like Göteborg and Halmstad. Because the
#"Denmark" are going in the same direction than the "Yugoslavia" groups on the
#second PC, we can make the same conclusion for the latter groups, namely that
#they over-represented in the South-West of Sweden. This calls for further
#investigation as to why. 

#4. 
explor::explor(PCA(dat, scale.unit = FALSE))
PCA(dat, scale.unit = TRUE)

#PCA is sensitive to magnitudes and one should therefore scale their data
#beforehand. In this case, because there are big numerical differences both
#between countries of origin (e.g., born in Sweden versus born in the 
#Philippins) and between municipalities (e.g., Stockholm versus any rural
#municipality), the PCA will overweight big cities or country groups. To ensure
#comparability, we need all variables/individuals to weigh equally, hence
#scaling. Note that this won't interfere with the structural differences within
#or between municipalities/ethnic groups. It will only erase the numerical
#differences between them.

#5. 
dat2 <- dat[!rownames(dat) %in% c("Stockholm", "Malmö", "Göteborg"),]
explor::explor(PCA(dat2))

# If we remove the three big cities, the explained variance of the first axis
#is lower yet still dominant (with 65%), and the second axis is essentially
#identical. The first and second reveal the same patterns with or without the
#three big cities, as other, nearby municipalities (e.g. Uppsala for the first
#axis, or Helsingborg in the second), are proxying the big cities. Further
#analysis could nonetheless investigate more axes, as those might reveal #additional patterns.  
