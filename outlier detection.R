############### GENERAL CLEANING ####################

##### Libraries #####
rm(list=ls())
library(tidyverse)
library(dplyr)
#library(tidytext)
#library(caret)
#library(cowplot)


##### Load Data #####
games <-read.csv('games_cleaned.csv')

##### Delete Useless Features #####
cols.dont.want <- c("X", "Unnamed..0", "description", "suggested_playerage", 
                    "suggested_language_dependence", "category",
                    "family", "designer", "artist", "publisher") 

games <- games[, ! names(games) %in% cols.dont.want, drop = F]


##### Extracting Mechanics #####
extract.mechanics <- function(games.input){
  games.output <- games.input %>%
    filter(!is.na(mechanic))

  games.sep1 <- games.output %>%
    separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
    mutate(cat12 = if_else(is.na(cat1), 'Not Av', cat1)) %>%
    mutate(cat12 = if_else(cat12=="", 'Not Av', cat12)) %>%
    dplyr::select(cat12)

  games.sep2 <- games.output %>%
    separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
    mutate(cat22 = if_else(is.na(cat2), 'Not Av', cat2)) %>%
    mutate(cat22 = if_else(cat22=="", 'Not Av', cat22)) %>%
    dplyr::select(cat22)

  games.sep3 <- games.output %>%
    separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
    mutate(cat32 = if_else(is.na(cat3), 'Not Av', cat3)) %>%
    mutate(cat32 = if_else(cat32=="", 'Not Av', cat32)) %>%
    dplyr::select(cat32)

  games.sep4 <- games.output %>%
    separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
    mutate(cat42 = if_else(is.na(cat4), 'Not Av', cat4)) %>%
    mutate(cat42 = if_else(cat42=="", 'Not Av', cat42)) %>%
    dplyr::select(cat42)

  games.sep5 <- games.output %>%
    separate(mechanic, c('cat1', 'cat2', 'cat3', 'cat4', 'cat5'), sep = ", ") %>%
    mutate(cat52 = if_else(is.na(cat5), 'Not Av', cat5)) %>%
    mutate(cat52 = if_else(cat52=="", 'Not Av', cat52)) %>%
    dplyr::select(cat52)
  
  print('Extraction Completed')
  
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
  df.cat1$id <- games.output$id

  df.cat2 <- data.frame(catmat2)
  df.cat2$id <- games.output$id

  df.cat3 <- data.frame(catmat3)
  df.cat3$id <- games.output$id

  df.cat4 <- data.frame(catmat4)
  df.cat4$id <- games.output$id

  df.cat5 <- data.frame(catmat5)
  df.cat5$id <- games.output$id

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
  
  print('Binding Dataframe')
  dm$V1 <- NULL
  games.output <- merge(games.output,dm, by='id')
  games.output = games.output %>% 
    rename(
      Prisoners.Dilemma = X.Prisoners.Dilemma.
    )

  games.output = games.output[, colSums(is.na(games.output)) != nrow(games.output)]
  games.output$Not.Av <- NULL
  print('Done!')
  
  return (games.output)
}
games <- suppressWarnings(extract.mechanics(games.input=games))

##### Selecting only relevant categories and mechanics #####

category.indices <- 23:92
mechanic.indices <- 93:197

tab.category = colSums(games[,category.indices], na.rm=TRUE)
summary(tab.category)
few_sample.category = names(tab.category)[tab.category < 300]

tab.mechanic = colSums(games[,mechanic.indices], na.rm=TRUE)
summary(tab.mechanic)
few_sample.mechanic = names(tab.mechanic)[tab.mechanic < 600]

games[,c(few_sample.category, few_sample.mechanic)] <- NULL

names(games)
category.indices <- 23:61
mechanic.indices <- 62:81

##### Boxplot of categories ####
df.cat.bplot <- data.frame()
for (i in 1:dim(games)[1]){
  for (j in category.indices){
    if (games[i,j] == 1) {
      l = list(name=games[i,]$name, category=names(games)[j], average=games[i,]$average)
      df.cat.bplot <- rbind(df.cat.bplot, l)
    }
  }
  
  if (i%%500 == 0) {
    print(i)
  }
}

library(hrbrthemes)
library(viridis)

df.cat.bplot$category = with(df.cat.bplot, reorder(category, average, median))
x11()
df.cat.bplot %>%
  mutate(category=fct_lump(category, 10)) %>%
  ggplot(aes(x=category, y=average, fill=category)) +
  geom_boxplot() +
  coord_flip() + 
  theme_ipsum() +
  theme(legend.position="none") + 
  xlab('') +
  ggtitle("Rating boxplot vs game category")
  

##### Boxplot of mechanics ####
df.mech.bplot <- data.frame()
for (i in 1:dim(games)[1]){
  for (j in mechanic.indices){
    if (games[i,j] == 1) {
      l = list(name=games[i,]$name, mechanic=names(games)[j], average=games[i,]$average)
      df.mech.bplot <- rbind(df.mech.bplot, l)
    }
  }
  
  if (i%%500 == 0) {
    print(i)
  }
}

library(hrbrthemes)
library(viridis)

df.mech.bplot$mechanic = with(df.mech.bplot, reorder(mechanic, average, median))
x11()
df.mech.bplot %>%
  mutate(mechanic=fct_lump(mechanic, 10)) %>%
  ggplot(aes(x=mechanic, y=average, fill=mechanic)) +
  geom_boxplot() +
  coord_flip() + 
  theme_ipsum() +
  theme(legend.position="none") + 
  xlab('') +
  ggtitle("Rating boxplot vs game mechanic")


##### Cleaning minage, playingtime, suggplayers #####

games <- games[games$minage != 0,]
games <- games[games$suggested_num_players != -1,]
games <- games[games$playingtime != 0,]
games[which((games$Worker.Placement) > 1), 'Worker.Placement'] <- 1


##### Some Plots ####
games <- games[games$year > 1900 & games$year < 2021,]

x11()
games %>%
  count(year) %>%
  arrange(desc(year)) %>%
  ggplot(aes(year, n)) +
  geom_line() + theme_bw() + theme(axis.text = element_text(size = 12), 
                                   axis.title = element_text(size = 14),
                                   plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) 

games %>%
  ggplot(aes(average)) +
  geom_histogram(binwidth=0.05) + theme_bw() + 
  geom_vline(aes(xintercept = mean(average)), colour="red")

x11()
games %>%
  ggplot(aes(averageweight)) +
  geom_histogram(binwidth=0.05) + theme_bw() +
  geom_vline(aes(xintercept = mean(averageweight)), colour="red")

games %>%
  filter(maxplaytime > 5, maxplaytime < 1000) %>%
  ggplot(aes(maxplaytime / 60)) +
  geom_histogram(binwidth=.25) +
  scale_x_log10(breaks= 2^seq(-2,4)) + theme_bw()

games %>%
  filter(suggested_num_players < 15) %>%
  ggplot(aes(suggested_num_players)) + geom_histogram(binwidth = 1) + theme_bw()

x11()
games %>%
  group_by(y5 = 5 * (year %/% 5)) %>%
  summarize(average_rating = mean(average)) %>%
  ggplot(aes(y5, average_rating)) + 
  ylab('Average Rating') + xlab('year') +
  geom_line() + theme_bw() + theme(axis.text = element_text(size = 12), 
                                   axis.title = element_text(size = 14),
                                   plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) 

##### Robust Outlier Detection on categories and mechanics ####
library(cbRw)
games2 <- games
for (ind in c(category.indices, mechanic.indices)){
  games[,ind] <- factor(games[, ind], levels = c('0', '1'))
}

value <- cbrw(games[, c(category.indices, mechanic.indices)])
out.ind <- value$score > quantile(value$score, 0.995)
out.name <- games$name[out.ind]
plot(games$averageweight, games$average, col=ifelse(value$score > quantile(value$score, 0.99), 'red', 'black'))

games[games$name==out.name[1], c(category.indices, mechanic.indices)]
games <- games[-out.ind,]

##### Clustering #######

compute.cooccurrence <- function(X) {
  X <- as.matrix(X)
  out <- crossprod(X)  # Same as: t(X) %*% X
  #diag(out) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)
  out
}

##### Category Clustering ####

library("corrplot")

for (ind in category.indices){
  games[,ind] <- as.numeric(games[,ind])-1
}
cor.mat <- round(cor(games[,category.indices]),2)

#observe correlation matrix
x11()
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

# compute distance matrix
co.oc <- compute.cooccurrence(games[,category.indices])
dist_mat <- 1/(co.oc+1)
dist_mat <- as.dist(dist_mat)

# observe dendrogram - 7 looks best
#x11()
dendo <- hclust(dist_mat, method='complete')
plot(dendo, main='complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.clusters <- rect.hclust(dendo, k=7)
category.clusters <- cutree(dendo, k=7)

# clusterize instances
clusterization.df <- function(df, reference.cluster, reference.indices, column.name) {
  clusterization.row <- function(df.row, reference.cluster){
    
    len = length(unique(reference.cluster))
    cluster.vec <- numeric(len)
    
    
    for (cl.ind in 1:len){
      cl.names <- names(reference.cluster[reference.cluster == cl.ind])
      cluster.vec[cl.ind] <- sum(df.row[cl.names])/length(cl.names)
    }
    
    assign.cluster <- which.max(cluster.vec)
    #cluster.vec
    assign.cluster
    
  }
  
  games[,column.name] <- apply(games[,reference.indices], 1, clusterization.row, reference.cluster=reference.cluster)
  games[,column.name] <- as.factor(games[,column.name])
  
  return (games)
}

games <- clusterization.df(df=games, reference.cluster = category.clusters, 
                           reference.indices = category.indices, column.name='category.cluster')
summary(games$category.cluster)

#write.csv(games, 'clusterized_games1.csv')

##### Mechanic Clustering ####

for (ind in mechanic.indices){
  games[,ind] <- as.numeric(games[,ind])-1
}
cor.mat <- round(cor(games[,mechanic.indices]),2)

#observe correlation matrix
#x11()
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

# compute distance matrix
mechanic.co.oc <- compute.cooccurrence(games[,mechanic.indices])
mechanic.dist_mat <- 1/(mechanic.co.oc+1)
mechanic.dist_mat <- as.dist(mechanic.dist_mat)

# observe dendrogram - 4 looks best
mechanic.dendo <- hclust(mechanic.dist_mat, method='complete')
plot(mechanic.dendo, main='complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
mechanic.rect.clusters <- rect.hclust(mechanic.dendo, k=4)
mechanic.clusters <- cutree(mechanic.dendo, k=4)

games <- clusterization.df(df=games, reference.cluster=mechanic.clusters, 
                           reference.indices=mechanic.indices, column.name='mechanic.cluster')
summary(games$mechanic.cluster)

#write.csv(games, 'clusterized_games2.csv')

##### Keeping only recent years and good numratings/numcomments #####
games <- games[games$year > 2000,]
games <- games[games$numratings > 150,]
games <- games[games$numcomments > 100,]

##### Robust statistics on wanting/owning ####
library(robustbase)
fit_MCD <- covMcd(x = games[, c('wanting', 'owned', 'average', 'averageweight')], alpha = .95, nsamp = "best")
fit_MCD
#plot(fit_MCD,classic=TRUE)


ind_best_subset <- fit_MCD$best
N <- nrow(games[, c('wanting', 'owned')])
p <- ncol(games[, c('wanting', 'owned')])


library(ggpubr)
games.plot <- cbind(games, status=ifelse(1:N%in%ind_best_subset,"inlier","outlier"))

owned.dplot <- ggdensity(games.plot, "owned", fill = "status",
                         palette = "jco") + rotate() + clean_theme()

wanting.dplot <- ggdensity(games.plot, "wanting", fill = "status",
                         palette = "jco") + clean_theme()

average.dplot <- ggdensity(games.plot, "average", fill = "status",
                           palette = "jco") + rotate() + clean_theme()

complexity.dplot <- ggdensity(games.plot, "averageweight", fill = "status",
                           palette = "jco") + clean_theme()

sp.wo <- ggscatter(games.plot, x = "wanting", y = "owned",
                #add = "reg.line",               # Add regression line
                #conf.int = TRUE,                # Add confidence interval
                color = 'status', 
                palette = "jco", # Color by groups "cyl"
                shape = "status"                   # Change point shape by groups "cyl"
)

sp.wa <- ggscatter(games.plot, x = "wanting", y = "average",
                   #add = "reg.line",               # Add regression line
                   #conf.int = TRUE,                # Add confidence interval
                   color = 'status', 
                   palette = "jco", # Color by groups "cyl"
                   shape = "status"                   # Change point shape by groups "cyl"
)

sp.ac <- ggscatter(games.plot, x = "averageweight", y = "average",
                   #add = "reg.line",               # Add regression line
                   #conf.int = TRUE,                # Add confidence interval
                   color = 'status', 
                   palette = "jco", # Color by groups "cyl"
                   shape = "status"                   # Change point shape by groups "cyl"
)

sp.oc <- ggscatter(games.plot, x = "averageweight", y = "owned",
                   #add = "reg.line",               # Add regression line
                   #conf.int = TRUE,                # Add confidence interval
                   color = 'status', 
                   palette = "jco", # Color by groups "cyl"
                   shape = "status"                   # Change point shape by groups "cyl"
)
x11()
ggarrange(NULL, complexity.dplot, wanting.dplot,
          owned.dplot, sp.oc, sp.wo, 
          average.dplot, sp.ac, sp.wa, 
          labels = c("A", "B", "C", 
                     'D', 'E', 'F', 
                     'G', 'H', 'I'),
          ncol = 3, nrow = 3, align='hv', common.legend = TRUE, legend = "top")

games <- games[games.plot$status == 'inlier',]

#write.csv(games, 'clusterized_games2.csv')

##### MCA #####
mca.indices <- c(23:37)#c(23:35)
for (ind in mca.indices){
  games[,ind] <- as.factor(games[, ind])
}
res.mca = MCA(games[,c(mca.indices, 13)], quanti.sup=6)


barplot(res.mca$eig[,1], main = "Eigenvalues", 
        names.arg = paste("Dim", 1:nrow(res.mca$eig), sep = ""))
abline(h = 1 / (length(mca.indices)), col='red')

plot(cumsum(res.mca$eig[,1]), type='o', axes=T, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')

options(ggrepel.max.overlaps = Inf)
plot(res.mca,invisible=c("ind"), selectMod="contrib 10", unselect=0.9)
plot(res.mca,invisible=c("ind"), axes = c(1, 3), selectMod="cos2 10", unselect=1)
plot(res.mca, choix="quanti.sup", axes = c(1, 3), unselect=1)

###### K-medoids Categories #####
library(cluster)

for (ind in c(category.indices, mechanic.indices)){
  games[,ind] <- factor(games[, ind], levels = c('0', '1'))
}

category.gower_dist <- 
  games %>% 
  # de-select account_id
  select(c(category.indices, 
           #mechanic.indices
           )) %>%   
  daisy(metric = "gower")

category.sil_width <- c(NA)

for (i in 2:8) {  
  set.seed(1234)
  category.pam_fit <- pam(category.gower_dist, diss = TRUE, k = i)  
  category.sil_width[i] <- category.pam_fit$silinfo$avg.width 
  cat("Done: ", i, 'Clusters\n')
}

category.sil_width %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  filter(rowid %in% c(2:8)) %>% 
  ggplot(aes(rowid, value)) +
  geom_line(colour  = 'black', size = 0.7) +
  geom_point(colour = 'black', size = 1.3) +
  theme_minimal() +
  labs(title = 'Silhouette Widths of k-medoid Clusters',
       x     = "Number of clusters",
       y     = 'Silhouette Width') +
  theme(plot.title = element_text(hjust = 0.5))

# visualization
set.seed(1234)
category.pam_fit <- 
  category.gower_dist %>% 
  # diss = TRUE to treat argument as dissimilarity matrix
  pam(k = 6, diss = TRUE)

library(Rtsne)
x11()
category.tsne_obj <- Rtsne(category.gower_dist, is_distance = TRUE)
category.tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(category.pam_fit$clustering)) %>% 
  
  # plot
  ggplot(aes(x = X, y = Y, colour = cluster)) +
  geom_point() +
  theme_light() +
  labs(title       = 't-SNE 2D Projections of k-medoid Clusters')  +
  theme(plot.title = element_text(hjust = 0.5))

category.clust_data <- games
category.pam_results <- category.clust_data %>%
  # append the cluster information onto clust_data
  mutate(cluster = category.pam_fit$clustering) %>% 
  # sort out variable order
  select(id, cluster, everything()) %>% 
  # attach some extra data features from the data_clean file
  left_join(select(games, 
                   c(id, average, averageweight)), by = 'id') %>%
  mutate_if(is.character, funs(factor(.)))

category.pam_results %>% 
  filter(cluster == 6) %>%
  select(c(average.x, averageweight.x, wanting, owned)) %>% 
  summary()


###### K-medoids Mechanics #####

for (ind in c(category.indices, mechanic.indices)){
  games[,ind] <- factor(games[, ind], levels = c('0', '1'))
}

mechanic.gower_dist <- 
  games %>% 
  # de-select account_id
  select(c(#category.indices, 
           mechanic.indices
  )) %>%   
  daisy(metric = "gower")

mechanic.sil_width <- c(NA)

for (i in 2:8) {  
  set.seed(1234)
  mechanic.pam_fit <- pam(mechanic.gower_dist, diss = TRUE, k = i)  
  mechanic.sil_width[i] <- mechanic.pam_fit$silinfo$avg.width 
  cat("Done: ", i, 'Clusters\n')
}

mechanic.sil_width %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  filter(rowid %in% c(2:8)) %>% 
  ggplot(aes(rowid, value)) +
  geom_line(colour  = 'black', size = 0.7) +
  geom_point(colour = 'black', size = 1.3) +
  theme_minimal() +
  labs(title = 'Silhouette Widths of k-medoid Clusters',
       x     = "Number of clusters",
       y     = 'Silhouette Width') +
  theme(plot.title = element_text(hjust = 0.5))

# visualization
set.seed(1234)
mechanic.pam_fit <- 
  mechanic.gower_dist %>% 
  # diss = TRUE to treat argument as dissimilarity matrix
  pam(k = 6, diss = TRUE)

mechanic.tsne_obj <- Rtsne(mechanic.gower_dist, is_distance = TRUE)
mechanic.tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(mechanic.pam_fit$clustering)) %>% 
  
  # plot
  ggplot(aes(x = X, y = Y, colour = cluster)) +
  geom_point() +
  theme_light() +
  labs(title       = 't-SNE 2D Projections of k-medoid Clusters')  +
  theme(plot.title = element_text(hjust = 0.5))

mechanic.clust_data <- games
mechanic.pam_results <- mechanic.clust_data %>%
  # append the cluster information onto clust_data
  mutate(cluster = mechanic.pam_fit$clustering) %>% 
  # sort out variable order
  select(id, cluster, everything()) %>% 
  # attach some extra data features from the data_clean file
  left_join(select(games, 
                   c(id, average, averageweight)), by = 'id') %>%
  mutate_if(is.character, funs(factor(.)))

mechanic.pam_results %>% 
  filter(cluster == 1) %>%
  select(c(average.x, averageweight.x, averageweight.y, wanting, owned)) %>% 
  summary()


##### Add column
games[,'category.cluster.kmed'] <- as.factor(category.pam_fit$clustering)
games[,'mechanic.cluster.kmed'] <- as.factor(mechanic.pam_fit$clustering)
write.csv(games, 'clusterized_games3.csv')