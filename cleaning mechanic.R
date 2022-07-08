# Libraries
library(tidyverse)
library(tidytext)
library(caret)
library(cowplot)
library(dplyr)

##### Loading #####
games <-read.csv('games_cleaned.csv')

##### cleaning mechanic ######
games <- games %>%
  filter(!is.na(category))

games.sep1 <- games %>%
  separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
  mutate(cat12 = if_else(is.na(cat1), 'Not Av', cat1)) %>%
  mutate(cat12 = if_else(cat12=="", 'Not Av', cat12)) %>%
  dplyr::select(cat12)

games.sep2 <- games %>%
  separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
  mutate(cat22 = if_else(is.na(cat2), 'Not Av', cat2)) %>%
  mutate(cat22 = if_else(cat22=="", 'Not Av', cat22)) %>%
  dplyr::select(cat22)

games.sep3 <- games %>%
  separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
  mutate(cat32 = if_else(is.na(cat3), 'Not Av', cat3)) %>%
  mutate(cat32 = if_else(cat32=="", 'Not Av', cat32)) %>%
  dplyr::select(cat32)

games.sep4 <- games %>%
  separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
  mutate(cat42 = if_else(is.na(cat4), 'Not Av', cat4)) %>%
  mutate(cat42 = if_else(cat42=="", 'Not Av', cat42)) %>%
  dplyr::select(cat42)

games.sep5 <- games %>%
  separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
  mutate(cat52 = if_else(is.na(cat5), 'Not Av', cat5)) %>%
  mutate(cat52 = if_else(cat52=="", 'Not Av', cat52)) %>%
  dplyr::select(cat52)

remove_first5 <- function(name) {
  new_name = substring(name, 6)
  return (new_name)
}

ChangeColnames <- function(x) {
  colnames(x) <- remove_first5(colnames(x))
  x
}

catmat1 <- model.matrix(~cat12-1, games.sep1)
catmat1 <- ChangeColnames(catmat1)

catmat2 <- model.matrix(~cat22-1, games.sep2)
catmat2 <- ChangeColnames(catmat2)

catmat3 <- model.matrix(~cat32-1, games.sep3)
catmat3 <- ChangeColnames(catmat3)

catmat4 <- model.matrix(~cat42-1, games.sep4)
catmat4 <- ChangeColnames(catmat4)

catmat5 <- model.matrix(~cat52-1, games.sep5)
catmat5 <- ChangeColnames(catmat5)

df.cat1 <- data.frame(catmat1)
df.cat1$id <- games$id

df.cat2 <- data.frame(catmat2)
df.cat2$id <- games$id

df.cat3 <- data.frame(catmat3)
df.cat3$id <- games$id

df.cat4 <- data.frame(catmat4)
df.cat4$id <- games$id

df.cat5 <- data.frame(catmat5)
df.cat5$id <- games$id

dm <-bind_rows(df.cat1, 
               df.cat2,
               df.cat3,
               df.cat4,
               df.cat5
) %>%
  # evaluate following calls for each value in the rowname column
  group_by(id) %>% 
  # add all non-grouping variables
  summarise_all(sum)

dm$V1 <- NULL
games <- merge(games,dm, by='id')
games = games %>% 
  rename(
    Prisoners.Dilemma = X.Prisoners.Dilemma.
  )

games = games[, colSums(is.na(games)) != nrow(games)]
games$Not.Av <- NULL

tab = colSums(games[,c(103:207)])
summary(tab)
few_sample = names(tab)[tab < 600]
games[,few_sample] <- NULL

##### Cleaning useless columns #####

cols.dont.want <- c("X", "Unnamed..0", 
                    "description", "suggested_playerage", 
                    "suggested_language_dependence", "category",
                    "family", "designer", "artist", "publisher") # if you want to remove multiple columns

games <- games[, ! names(games) %in% cols.dont.want, drop = F]
names(games)

##### Computing mechanic MCA ####
#mech.indices <- 33:170
mech.indices <- 93:112#103:170
library(FactoMineR)

for (ind in mech.indices){
  games[,ind] <- factor(games[, ind])
}

mca.indices <- c(mech.indices, 13)
res.mca = MCA(games[,mca.indices], quanti.sup=21)

barplot(res.mca$eig[,1], main = "Eigenvalues", 
        names.arg = paste("Dim", 1:nrow(res.mca$eig), sep = ""))
abline(h = 1 / (length(mca.indices)-1), col='red')

plot(cumsum(res.mca$eig[,1]), type='b', axes=T, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')

options(ggrepel.max.overlaps = Inf)
plot(res.mca,invisible=c("ind"), axes = c(1, 3), selectMod="contrib 10", unselect=0.9)
plot(res.mca,invisible=c("ind"), axes = c(1, 2), selectMod="cos2 10", unselect=1)


#res.hcpc = HCPC(res.mca)


##### Feature Clustering #####
library("corrplot")
mech.indices <- 93:105#92:136
for (ind in mech.indices){
  games[,ind] <- as.numeric(games[, ind])
}
cor.mat <- round(cor(games[,mech.indices]),2)

x11()
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

#----- Dissimilarity Matrix -----#
library(cluster) 
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
for (ind in mech.indices){
  games[,ind] <- as.factor(games[, ind])
}
games <- games[games$numratings > 150,]
set.seed(1234)
inds <- sample(dim(games)[1], 4000)
gower.dist <- daisy(games[ inds ,mech.indices], metric = c("gower"))

divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

#------------ AGGLOMERATIVE CLUSTERING ------------#
# I am looking for the most balanced approach
# Complete linkages is the approach that best fits this demand - I will leave only this one here, don't want to get it cluttered
# complete
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")

# Cluster stats comes out as list while it is more convenient to look at it as a table
# This code below will produce a dataframe with observations in columns and variables in row
# Not quite tidy data, which will require a tweak for plotting, but I prefer this view as an output here as I find it more comprehensive 
library(fpc)
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 10)
stats.df.divisive

stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 10) #complete linkages looks like the most balanced approach
stats.df.aggl

# --------- Choosing the number of clusters ---------#
# Using "Elbow" and "Silhouette" methods to identify the best number of clusters
# to better picture the trend, I will go for more than 7 clusters.
library(ggplot2)
# Elbow
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

#Agglomerative clustering,provides a more ambiguous picture
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

# agg
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

library("ggplot2")
library("reshape2")
library("purrr")
library("dplyr")
# let's start with a dendrogram
library("dendextend")
dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 7, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan", "cyan3", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 7")

# Time for the heatmap
# the 1st step here is to have 1 variable per row
# factors have to be converted to characters in order not to be dropped
clust.num <- cutree(aggl.clust.c, k = 7)
games.cl <- cbind(games[inds, c(1,mech.indices)], clust.num)
cust.long <- melt(data.frame(lapply(games.cl, as.character), stringsAsFactors=FALSE), 
                  id = c("id", "clust.num"), factorsAsStrings=T)
cust.long.q <- cust.long %>%
  group_by(clust.num, variable, value) %>%
  mutate(count = n_distinct(id)) %>%
  distinct(clust.num, variable, value, count)
# heatmap.c will be suitable in case you want to go for absolute counts - but it doesn't tell much to my taste
heatmap.c <- ggplot(cust.long.q, aes(x = clust.num, y = factor(value, levels = rep(c('0','1'), 109)))) +
  geom_tile(aes(fill = count))+
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
# calculating the percent of each factor level in the absolute count of cluster members
cust.long.p <- cust.long.q %>%
  group_by(clust.num, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(clust.num)
heatmap.p <- ggplot(cust.long.p, aes(x = clust.num, y = factor(value, levels = c('0','1')))) +
  
  geom_tile(aes(fill = perc), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  geom_hline(yintercept = 3.5) + 
  geom_hline(yintercept = 10.5) + 
  geom_hline(yintercept = 13.5) + 
  geom_hline(yintercept = 17.5) + 
  geom_hline(yintercept = 21.5) + 
  scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4")
heatmap.p
