#---------------------------------#
# Title: Code answers for Lab8    #
# Author: Maël Lecoursonnais      #
# Initial date: 2024-05-01        #
#---------------------------------#

# This script includes the code to answer 
# the exercises 1 and 2 of Lab8

# In this script, I give an outline of answers.
# If you want a detailed example on how one can
# analyze a PCA/MCA, look at the analysis in 
# Lab7_answers.R.

# Setup ----
Sys.setenv(LANG="en")
library(FactoMineR)
library(explor)

# Exercise 1. ----
dat <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2023/05/Valkompass2014_ShortRecoded_anon.csv", encoding = "UTF-8")

# 1.
str(dat)
collapse::descr(dat)

#Table -- 
qs <- grep("^Q", colnames(dat), value = TRUE)
t(sapply(dat[qs], table))

# Outline: the data set has 268 observations and 62 variables, indicating...

# 2.

#Access the variables about questions (start with Q)
active <- grep("^Q", colnames(dat), value = TRUE)

#Supplementary variables
supp <- c("Gender", "YearBirth", "PoliticalParty", "Income2012",
          "PoliticalSelfPositioning", "PersonalCampaign", 
          "OccupationalStatus", "HighestDiploma")

#Reduce the data to needed variables
MCA_data <- dat[, c(supp, active)]

#Perform the MCA: use `quanti.sup` for numeric suppl. variables, 
#`quali.sup` for categorical suppl. variables.
res <- MCA(MCA_data, quanti.sup = c(2, 4), 
           quali.sup = c(1, 3, 5:8), graph = FALSE)

explor(res)

# Outline: Using the elbow method, I would choose 3 PCs, because the eigenvalues
#are marginal afterwards. (2 or 4 could work too here). The first dimension (35%)
#clearly opposes Left and Right political parties. Pretty much all questions are
#contributing to this PC, but the most cleaving ones (highest contributions) are
#about gender quotas in companies or nuclear power. The second dimension (11%)
#opposes the far-right politicians with the rest, with questions around
#immigration and EU being the most contributing. The third dimension opposes the
#Social Democrats and the Left, Green and Center Party, with questions around
#Internet freedom being the most diverging between the two clusters (highest
#contributions).

#3. 
idx <- is.na(dat$Income2008)
dat_na <- dat
dat_na[idx, active[c(1:5, 17:20)]] <- NA

MCA_data2 <- dat_na[, c(supp, active)]
res <- MCA(MCA_data2, quali.sup = c(1, 3, 5:8), quanti.sup = c(2, 4), graph = F)
explor(res)

# Outline of answer: NAs becomes a dimension on its own (the second), where there is an opposition between non-NAs and NAs, which is common in PCA. Now, in this case, it shows a specific mechanism because it is simulated. But in many other cases, it can be uninformative, in which case one can use missing data imputation.

# Exercice 2. ----

#1. 
cult <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2023/05/CultPract.csv")
cult$Diploma[cult$Diploma %in% c("CAP-BEP", "CEP")] <- "Vocational < Bac"
cult$Diploma[cult$Diploma == "BEPC"] <- "No high school"
cult$PCS[cult$PCS == "Agriculteurs"] <- "Farmers"

#2. 

#Find a way to get a percentage table for the active variables
sapply(cult, function(x) round(prop.table(table(x)), 2))
#Logical vector: are all categories more than 5%?
sapply(cult, function(x) all(prop.table(table(x)) > 0.05))

#3.
res <- MCA(cult, quali.sup = 7:10)
plot(res$eig[, 3])

explor::explor(res)

#Given the scree plot, the data is very complex, and principal components have low share of explained variance. We will stick in this analysis to the first four, representing 20% of the variance. PC1: Age dimension; students versus old people. PC2: single versus family activities. PC3: Educated versus non-educated. PC4: Men and women. All of these dimensions highlights oppositions; continuum along which cultural practices differ between socioeconomic groups. 



