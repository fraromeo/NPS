###### Clustering

compute.cooccurrence <- function(X) {
  X <- as.matrix(X)
  out <- crossprod(X)  # Same as: t(X) %*% X
  #diag(out) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)
  out
}

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
cat.indices <- 23:61#92:136
cor.mat <- round(cor(games[,cat.indices]),2)
x11()
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)


co.oc <- compute.cooccurrence(games[,cat.indices])
dist_mat <- 1/(co.oc+1)
dist_mat <- as.dist(dist_mat)

# rownames(dist_mat) <- colnames(games[,first_cat : last_cat])
dendo <- hclust(dist_mat, method='complete')
plot(dendo, main='complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.clusters <- rect.hclust(dendo, k=9)
categories.clusters <- cutree(dendo, k=9)

# try to merge features

# function that assigns each game to a cluster
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

  games$cluster <- apply(games[,23:61], 1, clusterization.row, reference.cluster=categories.clusters)
  games$cluster <- as.factor(games$cluster)
  
  return (games)
}

games <- clusterization.df(df=games, reference.cluster = categories.clusters, cat.ind = cat.indices)
summary(games$cluster)
