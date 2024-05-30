# balance of topics within PCA
# make sure to take a look
# --> acknowledge or fix
# 
# maybe us MISMDA package? for dealing with missings


library(FactoMineR)
library(explor)
library(mice)
library(tidyverse)


hfi2008_2016 <- read_csv(paste0(here::here(),"/DS_FINAL/HFI/hfi2008_2016.csv"))
# maybe religious freedom and labour market regulation?

str(hfi2008_2016)

# hfi2008_2016 |> 
#   group_by(countries) |> 
#   summarise(across(everything(), ~ sum(is.na(.x)))) |> 
#   rowwise() |> 
#   mutate(total_missing = sum(c_across(where(is.numeric))),
#          missing_perc = round(total_missing/(123*8)*100, 2)) |> 
#   relocate(total_missing, .after = countries) |> 
#   relocate(missing_perc, .after = total_missing) |> 
#   View()

(percent_missing <- hfi2008_2016 |> 
  group_by(year, countries) |> 
  summarise(across(everything(), ~ sum(is.na(.x)))) |> 
  rowwise() |> 
  mutate(total_missing = sum(c_across(where(is.numeric))),
         missing_perc = round((total_missing/123)*100, 2)) |> 
  relocate(total_missing, .after = countries) |> 
  relocate(missing_perc, .after = total_missing))
# interesting here is that across years the same countries tend to have the same number of missing -> MAR not met

to_filter_countries_years <- percent_missing |> filter(missing_perc > 42) |> select(year, countries) |> as.data.frame()

# now check which year has the most countries - it's 2016 and the number of responses has increased gradually since 2008.
anti_join(hfi2008_2016, to_filter_countries_years) |> group_by(year) |> summarise(n = n())

df <- anti_join(hfi2008_2016, to_filter_countries_years)

# overview of which variables are missing in what years among the already subset data
# since the total number of of countries in 2016 in 162, a missing_percentage is calculated
df |> group_by(year) |> summarise(across(everything(), ~ sum(is.na(.)))) |> View()
(to_filter_vars <- df |> filter(year == 2016) |> summarise(across(everything(), ~ sum(is.na(.)))) |> t() |> as.data.frame() |> mutate(missing_perc = round(((V1/162)*100), 2)) |> arrange(missing_perc))
to_filter_vars <- to_filter_vars |> filter(missing_perc > 42) |> rownames()
df <- df |> filter(year == 2016) |>  select(!any_of(to_filter_vars))



# impute data based on the entire dataset but then only take 2016 for PCA

# where are the missing values?

# hard to see but you know
md.pattern(df)

df_imp <- mice(df)

complete_df <- complete(df_imp)
# -> 4-5 sentencens
# appendix with values of columns you're keeping before and after imputation

# save this cause it takes a while to run each time
write_rds(complete_df, "complete_df.rds")

complete_df <- read_rds("complete_df.rds")
# PCA

# other supplementary variables should be scores calculated from the existing data
# I argue that this information is not useful to exploratory data analysis
# as it is a product of the other data and provides no unique information
(quanti_sup <- complete_df |> select(contains(c("score", "rank", "quartile"))) |> colnames())

# as a final step before PCA rename the rownames to the countries
complete_df <- complete_df |> column_to_rownames(var = "countries")
# double check everything is numeric - 3 columns are not (the country indicators) - 
# these shall be added to the supplementary information during PCA
## (complete_df |> ncol() - complete_df |> select(where(is.numeric)) |> ncol())
# actually I've decided to just remove all quali sups aside from region
complete_df <- complete_df |> select(-c(ISO_code, year))
# other supplementary variables should be scores calculated from the existing data
# I argue that this information is not useful to exploratory data analysis
# as it is a product of the other data and provides no unique information
(quanti_sup <- complete_df |> select(contains(c("score", "rank", "quartile"))) |> colnames())


res <- PCA(complete_df, quali.sup = "region", quanti.sup = quanti_sup, graph = FALSE)

explor(res)

# HClust

newDatCluster <- res$ind$coord[, 1:2]

hc <- dist(newDatCluster, method = "euclidean") #Distance
res.hc <- hclust(hc, method = "ward.D2") #Linkage
plot(res.hc, cex = 0.5, hang = -1)
rect.hclust(res.hc, k = 3, border = 2:7)
factoextra::fviz_nbclust(complete_df, hcut, hc_method = "ward.D2", method = c("silhouette")) # REPLICATE THIS WITH LINE 85 and 83 from lab9

cluster <- cutree(res.hc, 3)
plot(newDatCluster[, 1:2], col = cluster)

# Questions:

# question about scaling:
# how does it work when the range of one of my 0-10 vars is actually 0-9 or 2-10 
# in my data? does this matter? can I "manually scale"? 

# as a reminder: with the contributions from PCA you can go on to do clustering?
## -> dimensions of the new coords are used
## -> when using clustering after then use more dimensions!! -> probably like 90% of explained variance

# how to go on from clustering results? --> regressions etc?

# PCA: explanantoon
# opposing variables
# who contributes
# cos^2
# and coordinates
# but just do this for 1 and 2 PC
# very briefly for further PCs

# generally about the paper -> focus on exploration & visualization?

# finding a research question: base it on PCA axes? I was interested in the connection
# between freedom and minimum wage but I guess this kind of gets lost with PCA.
## different definitions of freedom?? level them against each other
## maybe evolution of concepts over time
## how does it change across countries?
## cultural blah

# can I have another 10mins on the 5th?
# 
# 
# structure:
# explicit about each section
# introduction with RQ
# literature review after
# data section where you present data as though they didn't know it:
## missing data
# methods present them 
# --> "explanation to classmates"
# did you make any choices -> why PCA? why PCA good?
# results:
## state number of axes and number of clusters
## have detailled interpretataions of 2 PCs
# discussion: tie it back together
## how do these things relate

