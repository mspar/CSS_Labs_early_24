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

# load in data 
hfi2008_2016 <- read_csv(paste0(here::here(),"/DS_FINAL/HFI/hfi2008_2016.csv"))

# have a little glance
str(hfi2008_2016)

# checking missings of countries by year
(percent_missing <- hfi2008_2016 |> 
  group_by(year, countries) |> 
  summarise(across(everything(), ~ sum(is.na(.x)))) |> 
  rowwise() |> 
  mutate(total_missing = sum(c_across(where(is.numeric))),
         missing_perc = round((total_missing/123)*100, 2)) |> 
  relocate(total_missing, .after = countries) |> 
  relocate(missing_perc, .after = total_missing))
# interesting here is that across years the same countries tend to have the same number of missing -> MAR not met

# creating mini data frame to filter with
to_filter_countries_years <- percent_missing |> filter(missing_perc > 42) |> select(year, countries) |> as.data.frame()

# only keep countries with sufficient responses
# anti_join(hfi2008_2016, to_filter_countries_years) |> group_by(year) |> summarise(n = n())
df <- anti_join(hfi2008_2016, to_filter_countries_years)

# overview of which variables have how many missings in which years among the already subset data?
df |> group_by(year) |> summarise(across(everything(), ~ sum(is.na(.)))) |> View()
(to_filter_vars16 <- df |> filter(year == 2016) |> summarise(across(everything(), ~ sum(is.na(.)))) |> t() |> as.data.frame() |> mutate(missing_perc = round(((V1/162)*100), 2)) |> arrange(missing_perc))
(to_filter_vars08 <- df |> filter(year == 2008) |> summarise(across(everything(), ~ sum(is.na(.)))) |> t() |> as.data.frame() |> mutate(missing_perc = round(((V1/162)*100), 2)) |> arrange(missing_perc))

# get the union of offending variables in both 2008 and 2016 and filter the data frames
to_filter_vars16 <- to_filter_vars16 |> filter(missing_perc > 42) |> rownames()
to_filter_vars08 <- to_filter_vars08 |> filter(missing_perc > 42) |> rownames()
to_filter_vars_general <- union(to_filter_vars08,to_filter_vars16)
df16 <- df |> filter(year == 2016) |>  select(!any_of(to_filter_vars_general))
df08 <- df |> filter(year == 2008) |>  select(!any_of(to_filter_vars_general))
# only keep countries appearing in both data sets
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
md.pattern(df16)
md.pattern(df08)

# impute
df_imp16 <- mice(df16)
df_imp08 <- mice(df08)
complete_df16 <- complete(df_imp16)
complete_df08 <- complete(df_imp08)

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
(complete_df |> ncol() - complete_df |> select(where(is.numeric)) |> ncol())

# actually I've decided to just remove all quali sups aside from region
complete_df16 <- complete_df16 |> select(-c(ISO_code, year))
complete_df08 <- complete_df08 |> select(-c(ISO_code, year))

# run PCAs
res16 <- PCA(complete_df16, quali.sup = "region", quanti.sup = quanti_sup, graph = FALSE)
res08 <- PCA(complete_df08, quali.sup = "region", quanti.sup = quanti_sup, graph = FALSE)

# explor for visualization
explor(res16)
explor(res08)

# PCA visualization plan:
## -> Scree plots

# only take top 15 components
res08$eig2 <- res08$eig |> as.data.frame() |> arrange(desc(`percentage of variance`)) |> slice_head(n = 15)
res16$eig2 <- res16$eig |> as.data.frame() |> arrange(desc(`percentage of variance`)) |> slice_head(n = 15)

# plot with patchwork
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

# this entire part does more than is used later on

newDatCluster16 <- res16$ind$coord[, 1:2]

complete_df16 <- complete_df16 |> select(where(is.numeric)) |> select(-any_of(quanti_sup))

hc16 <- dist(newDatCluster16, method = "euclidean") #Distance
res.hc16 <- hclust(hc16, method = "ward.D2") #Linkage
plot(res.hc16, cex = 0.5, hang = -1)
(clust_16 <- rect.hclust(res.hc16, k = 3, border = 2:7))
# important! indices for decision on number of clusters
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

# this is because the clusters do not have the right order/color in the two plots
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
# important! indices for decision on number of clusters
nbclust <- NbClust(data = scale(newDatCluster08), distance = "euclidean", min.nc = 2, max.nc = 5, method = "ward.D2")

cluster08 <- cutree(res.hc08, 3)
plot(newDatCluster08[, 1:2], col = cluster08)



data("World")

## left join cluster belonging
name <- res.hc08$labels |> as.data.frame() |> rename(name = `res.hc08$labels`)
unname(cluster08) |> as.data.frame() |> rename(Clusters = `unname(cluster08)`) |> cbind(name) -> suppl_df08

World08 <- left_join(World, suppl_df08, by = join_by(name)) |> arrange(Clusters)

# this is because the clusters do not have the right order/color in the two plots
World08 <- World08 |> mutate(Clusters = as.factor(if_else(Clusters == 1, 3, if_else(Clusters == 3, 1, Clusters))))

clust_word_08 <- tm_shape(World08) +
  tm_polygons("Clusters", palette = hcl.colors(3, "ag_GrnYl")) +
  tm_layout(main.title = "Clustering Human Freedom - 2008") +
  tm_credits("Data: Human Freedom Index, 2008", fontface = "italic", align = "right")

tmap_save(clust_word_08, filename = "clust_word_08.png", height = 8.5, width = 12, dpi = 900)
