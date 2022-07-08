##### Intro ####

rm(list=ls())

##### Libraries ####

library(tidyverse)
library(tidytext)
library(caret)
library(cowplot)
library(dplyr)
library(GGally)
library(DepthProc)
library(aplpack)
library(splines)
library(mgcv)

##### Functions ####

compute.cooccurrence <- function(X) {
  X <- as.matrix(X)
  out <- crossprod(X)  # Same as: t(X) %*% X
  #diag(out) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)
  out
}
##### Begin ####

games <- readRDS("data.RDS")

glimpse(games)
summary(games)

dim(games)
dim(games[games$year > 1990, ]) 

category.indices <- 31:100
mat_cooccur <- compute.cooccurrence(games[,category.indices])

write.csv(games, 'games_da_ripulire.csv')

##### MCA ####
games <- read.csv('games_cleaned.csv')
category.indices <- 33:102
library(FactoMineR)
table(games[,31], games[,32], games[,33])

for (ind in category.indices){
  games[,ind] <- factor(games[, ind])
}

mca.indices <- c(category.indices, 23)
res.mca = MCA(games[,mca.indices], quanti.sup=71)
plot.MCA(res.mca, invisible=c("var","quali.sup"), cex=0.7)
plot.MCA(res.mca, invisible=c("ind","quali.sup"), cex=0.7)
plot.MCA(res.mca, invisible=c("ind"))
#plot.MCA(res.mca, invisible=c("ind", "var"))

plot.MCA(res.mca, choix = "var", invisible=c('ind'))

barplot(res.mca$eig[,1], main = "Eigenvalues", 
        names.arg = paste("Dim", 1:nrow(res.mca$eig), sep = ""))
abline(h = 1 / (length(mca.indices)-1), col='red')

plot(cumsum(res.mca$eig[,1]), type='o', axes=T, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')

plot(res.mca, choix = "var", axes = c(1, 2), select=('coord 10'), graph.type='ggplot', unselect=1)
plot(res.mca, choix = "var", axes = c(1, 2), select=("contrib 10"), unselect=1)
plot(res.mca, choix = "var", axes = c(1, 2), selectMod= "cos2 5", unselect=1)

options(ggrepel.max.overlaps = Inf)
plot(res.mca,invisible=c("ind"), selectMod="contrib 10", unselect=0.9)
plot(res.mca,invisible=c("ind"), axes = c(1, 3), selectMod="cos2 10", unselect=1)
plot(res.mca, choix="quanti.sup", axes = c(1, 5), unselect=1)

##### other trials
cols.dont.want <- c("X", "Unnamed..0", 
                    "description", "suggested_playerage", 
                    "suggested_language_dependence", "category",
                    "family", "designer", "artist", "publisher") # if you want to remove multiple columns

games <- games[, ! names(games) %in% cols.dont.want, drop = F]
names(games)

for (ind in c(23:92)){
  games[,ind] <- as.numeric(games[, ind])-1
}

# keeping few columns
games <- read.csv('games_cleaned.csv')
cols.dont.want <- c("X", "Unnamed..0", 
                    "description", "suggested_playerage", 
                    "suggested_language_dependence", "category",
                    "family", "designer", "artist", "publisher") # if you want to remove multiple columns

games <- games[, ! names(games) %in% cols.dont.want, drop = F]
names(games)
tab = colSums(games[,c(23:92)])
summary(tab)
few_sample = names(tab)[tab < 300]
games[,few_sample] <- NULL
names(games)

# correlation
library("corrplot")
mech.indices <- 23:61#92:136
for (ind in mech.indices){
  games[,ind] <- as.numeric(games[, ind])
}
cor.mat <- round(cor(games[,mech.indices]),2)

x11()
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

co.oc <- compute.cooccurrence(games[,mech.indices])

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


###### clustering #####
games <- read.csv('games_cleaned.csv')
cols.dont.want <- c("X", "Unnamed..0", 
                    "description", "suggested_playerage", 
                    "suggested_language_dependence", "category",
                    "family", "designer", "artist", "publisher") # if you want to remove multiple columns

games <- games[, ! names(games) %in% cols.dont.want, drop = F]
names(games)
tab = colSums(games[,c(23:92)])
summary(tab)
few_sample = names(tab)[tab < 300]
games[,few_sample] <- NULL
names(games)

# correlation
library("corrplot")
mech.indices <- 23:61#92:136
cor.mat <- round(cor(games[,mech.indices]),2)
x11()
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)


co.oc <- compute.cooccurrence(games[,mech.indices])
dist_mat <- 1/(co.oc+1)
dist_mat <- as.dist(dist_mat)

# rownames(dist_mat) <- colnames(games[,first_cat : last_cat])
dendo <- hclust(dist_mat, method='complete')
plot(dendo, main='complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.clusters <- rect.hclust(dendo, k=10)
categories.clusters <- cutree(dendo, k=10)

# try to merge features

clusterization <- function(row, reference.cluster){
  
  cluster.vec <- numeric(length(unique(reference.cluster)))
  print(cluster.vec)
  
  for (cl.ind in 1:length(cluster.like)){
    cl.names <- names(reference.cluster[reference.cluster == cl.ind])
    cluster.vec[cl.ind] <- rowSums(first[cl.names])/length(cl.names)
  }
  
  print(cluster.vec)
  assign.cluster <- which.max(cluster.vec)
  return (cluster.vec)
  
}


t(apply(games[1:5,], 1, clusterization, reference.cluster=categories.clusters))

###
first = games[16,]
reference = categories.clusters
cluster.like <- numeric(length(unique(reference)))
for (cl.ind in 1:length(cluster.like)){
  cl.names <- names(reference[reference == cl.ind])
  cluster.like[cl.ind] <- rowSums(first[cl.names])/length(cl.names)
}
first
