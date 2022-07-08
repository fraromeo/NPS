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

tab.category = colSums(games[,category.indices])
summary(tab.category)
few_sample.category = names(tab.category)[tab.category < 300]

tab.mechanic = colSums(games[,mechanic.indices])
summary(tab.mechanic)
few_sample.mechanic = names(tab.mechanic)[tab.mechanic < 600]

games[,c(few_sample.category, few_sample.mechanic)] <- NULL

names(games)
category.indices <- 23:61
mechanic.indices <- 62:81
##### Cleaning minage, playingtime, suggplayers #####

games <- games[games$minage != 0,]
games <- games[games$suggested_num_players != -1,]
games <- games[games$playingtime != 0,]

##### Robust Outlier Detection on categories and mechanics ####
library(cbRw)

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
#x11()
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

# compute distance matrix
co.oc <- compute.cooccurrence(games[,category.indices])
dist_mat <- 1/(co.oc+1)
dist_mat <- as.dist(dist_mat)

# observe dendrogram - 7 looks best
dendo <- hclust(dist_mat, method='complete')
plot(dendo, main='complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.clusters <- rect.hclust(dendo, k=7)
category.clusters <- cutree(dendo, k=7)

# clusterize instances
clusterization.df <- function(df, reference.cluster, cat.ind) {
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
  
  games$cluster <- apply(games[,category.indices], 1, clusterization.row, reference.cluster=reference.cluster)
  games$cluster <- as.factor(games$cluster)
  
  return (games)
}

games <- clusterization.df(df=games, reference.cluster = category.clusters, cat.ind = category.indices)
summary(games$cluster)

write.csv(games, 'clusterized_games1.csv')
