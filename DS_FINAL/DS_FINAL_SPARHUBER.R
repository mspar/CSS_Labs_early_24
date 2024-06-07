# balance of topics within PCA
# make sure to take a look
# --> acknowledge or fix
# 
# maybe us MISMDA package? for dealing with missings


library(FactoMineR)
library(explor)
library(mice)
library(tidyverse)
library(NbClust)
library(factoextra)
library(cluster)
library(tmap)
library(explor)
library(patchwork)

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
(to_filter_vars16 <- df |> filter(year == 2016) |> summarise(across(everything(), ~ sum(is.na(.)))) |> t() |> as.data.frame() |> mutate(missing_perc = round(((V1/162)*100), 2)) |> arrange(missing_perc))
(to_filter_vars08 <- df |> filter(year == 2008) |> summarise(across(everything(), ~ sum(is.na(.)))) |> t() |> as.data.frame() |> mutate(missing_perc = round(((V1/162)*100), 2)) |> arrange(missing_perc))

to_filter_vars16 <- to_filter_vars16 |> filter(missing_perc > 42) |> rownames()
to_filter_vars08 <- to_filter_vars08 |> filter(missing_perc > 42) |> rownames()
to_filter_vars_general <- union(to_filter_vars08,to_filter_vars16)
df16 <- df |> filter(year == 2016) |>  select(!any_of(to_filter_vars_general))
df08 <- df |> filter(year == 2008) |>  select(!any_of(to_filter_vars_general))

df16 <- df16 |> filter(ISO_code %in% df08$ISO_code)

# make sure to also remove biased economic vars and tallies
econ_remove <- df08 |> select(starts_with(c("ef_government", "ef_money", "ef_trade_tariffs", "ef_trade_regulatory", "ef_regulation_credit",
                                          "ef_regulation", "ef_trade_movement_capital", "ef_trade_movement_foreign"))) |> colnames()
tally_remove <- df08 |> select(c("ef_legal", "ef_trade", "ef_trade_movement" , "pf_rol", "pf_ss_disappearances", "pf_ss_women", "pf_ss", "pf_movement", "pf_expression", "pf_religion", "pf_association",
           "pf_identity", "pf_identity_sex")) |> colnames()
df16 <- df16 |> select(!any_of(c(econ_remove, tally_remove)))
df08 <- df08 |> select(!any_of(c(econ_remove, tally_remove)))


# impute missing values

# where are the missing values?

# hard to see but you know
md.pattern(df16)
md.pattern(df08)

df_imp16 <- mice(df16)
df_imp08 <- mice(df08)

complete_df16 <- complete(df_imp16)
complete_df08 <- complete(df_imp08)
# -> 4-5 sentencens
# appendix with values of columns you're keeping before and after imputation

# save this cause it takes a while to run each time
write_rds(complete_df16, "complete_df16.rds")
write_rds(complete_df08, "complete_df08.rds")

complete_df16 <- read_rds("complete_df16.rds")
complete_df08 <- read_rds("complete_df08.rds")
# PCA

# other supplementary variables should be scores calculated from the existing data
# I argue that this information is not useful to exploratory data analysis
# as it is a product of the other data and provides no unique information
(quanti_sup <- complete_df16 |> select(contains(c("score", "rank", "quartile"))) |> colnames())

# as a final step before PCA rename the rownames to the countries
complete_df16 <- complete_df16 |> column_to_rownames(var = "countries")
complete_df08 <- complete_df08 |> column_to_rownames(var = "countries")
# double check everything is numeric - 3 columns are not (the country indicators) - 
# these shall be added to the supplementary information during PCA
## (complete_df |> ncol() - complete_df |> select(where(is.numeric)) |> ncol())
# actually I've decided to just remove all quali sups aside from region
complete_df16 <- complete_df16 |> select(-c(ISO_code, year))
complete_df08 <- complete_df08 |> select(-c(ISO_code, year))
# other supplementary variables should be scores calculated from the existing data
# I argue that this information is not useful to exploratory data analysis
# as it is a product of the other data and provides no unique information
(quanti_sup <- complete_df16 |> select(contains(c("score", "rank", "quartile"))) |> colnames())


res16 <- PCA(complete_df16, quali.sup = "region", quanti.sup = quanti_sup, graph = FALSE)
res08 <- PCA(complete_df08, quali.sup = "region", quanti.sup = quanti_sup, graph = FALSE)

explor(res16)
explor(res08)

# PCA visualization plan:
## -> Scree plot
plot(res16$eig[, 2], type = "o") # -> find nicer way to plot this (there are)
plot(res08$eig[, 2], type = "o")

res08$eig2 <- res08$eig |> as.data.frame() |> arrange(desc(`percentage of variance`)) |> slice_head(n = 15)
res16$eig2 <- res16$eig |> as.data.frame() |> arrange(desc(`percentage of variance`)) |> slice_head(n = 15)

ggplot(res08$eig2, aes(x = factor(1:nrow(res08$eig2)), y = `percentage of variance`)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_line(aes(y = `percentage of variance`, group = 1), size = 1, color = "blue") +
  geom_point(aes(y = `percentage of variance`), size = 2, color = "red") +
  scale_y_continuous(limits = c(0, 32.5), breaks = seq(0, 30, by = 5)) +
  labs(
    title = "2008",
    x = "Principal Components",
    y = "Explained Variance"
  ) +
  theme_minimal() +
ggplot(res16$eig2, aes(x = factor(1:nrow(res16$eig2)), y = `percentage of variance`)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_line(aes(y = `percentage of variance`, group = 1), size = 1, color = "blue") +
  geom_point(aes(y = `percentage of variance`), size = 2, color = "red") +
  scale_y_continuous(limits = c(0, 32.5), breaks = seq(0, 30, by = 5)) +
  labs(
    title = "2016",
    x = "Principal Components",
    y = "Explained Variance"
  ) +
  theme_minimal() +
  plot_layout(axis_titles = "collect")








# HCLUST
## WORK THROUGH HIERARCHICAL CLUSTERING AGAIN JUST TO MAKE SURE I GOT IT RIGHT

newDatCluster16 <- res16$ind$coord[, 1:2]

complete_df16 <- complete_df16 |> select(where(is.numeric)) |> select(-any_of(quanti_sup))

hc16 <- dist(newDatCluster16, method = "euclidean") #Distance
res.hc16 <- hclust(hc16, method = "ward.D2") #Linkage
plot(res.hc16, cex = 0.5, hang = -1)
(clust_16 <- rect.hclust(res.hc16, k = 3, border = 2:7))
# REPLICATE THIS WITH LINE 85 and 83 from lab9
nbclust <- NbClust(data = scale(complete_df16), distance = "euclidean", min.nc = 2, max.nc = 5, method = "ward.D2")


cluster16 <- cutree(res.hc16, 3)
plot(newDatCluster16[, 1:2], col = cluster16)


# mapping test
data("World")

tmap_options(output.dpi = 900)
## left join cluster belonging
name <- res.hc16$labels |> as.data.frame() |> rename(name = `res.hc16$labels`)
unname(cluster16) |> as.data.frame() |> rename(Clusters = `unname(cluster16)`) |> cbind(name) -> suppl_df16

World16 <- left_join(World, suppl_df16, by = join_by(name)) |> arrange(Clusters)

World16 <- World16 |> mutate(Clusters = as.factor(if_else(Clusters == 1, 2, if_else(Clusters == 2, 1, Clusters))))

clust_word_16 <- tm_shape(World16) +
  tm_polygons("Clusters", palette = hcl.colors(3, "ag_GrnYl")) +
  tm_layout(main.title = "Clustering Human Freedom - 2016") +
  tm_credits("Data: Human Freedom Index, 2016", fontface = "italic", align = "right")

tmap_save(clust_word_16, filename = "clust_word_16.png", height = 8.5, width = 12, dpi = 900)


# 2008


newDatCluster08 <- res08$ind$coord[, 1:2]

complete_df08 <- complete_df08 |>  select(where(is.numeric)) |> select(-any_of(quanti_sup))

hc08 <- dist(newDatCluster08, method = "euclidean") #Distance
res.hc08 <- hclust(hc08, method = "ward.D2") #Linkage
plot(res.hc08, cex = 0.5, hang = -1)
rect.hclust(res.hc08, k = 3, border = 2:7)
nbclust <- NbClust(data = scale(newDatCluster08), distance = "euclidean", min.nc = 2, max.nc = 5, method = "ward.D2")
# REPLICATE THIS WITH LINE 85 and 83 from lab9

cluster08 <- cutree(res.hc08, 3)
plot(newDatCluster08[, 1:2], col = cluster08)


# mapping test
data("World")

## left join cluster belonging
name <- res.hc08$labels |> as.data.frame() |> rename(name = `res.hc08$labels`)
unname(cluster08) |> as.data.frame() |> rename(Clusters = `unname(cluster08)`) |> cbind(name) -> suppl_df08

World08 <- left_join(World, suppl_df08, by = join_by(name)) |> arrange(Clusters)

World08 <- World08 |> mutate(Clusters = as.factor(if_else(Clusters == 1, 3, if_else(Clusters == 3, 1, Clusters))))

clust_word_08 <- tm_shape(World08) +
  tm_polygons("Clusters", palette = hcl.colors(3, "ag_GrnYl")) +
  tm_layout(main.title = "Clustering Human Freedom - 2008") +
  tm_credits("Data: Human Freedom Index, 2008", fontface = "italic", align = "right")

tmap_save(clust_word_08, filename = "clust_word_08.png", height = 8.5, width = 12, dpi = 900)

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
## https://plato.stanford.edu/entries/liberty-positive-negative/

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
