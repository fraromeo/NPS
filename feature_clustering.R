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
#x11()
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

summary(games$numratings)


####### some analysis #####
games <- games[games$numratings > 100,]
fit <- lm(games$average ~ games$numratings * games$averageweight * games$cluster)
summary(fit)

plot(fit)


y <- games$average
x <- games$averageweight
local_model = npreg(y ~ x,
                    ckertype = 'epanechnikov',
                    bws = 0.5) # set the bandwidth



x.grid=data.frame(x=seq(range(x)[1],range(x)[2],by=0.1))
preds=predict(local_model, newdata=x.grid, se=T)
se.bands=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
plot(x, y, xlim = range(x.grid$x), cex=.5, col="darkgrey")
lines(x.grid$x, preds$fit, lwd =2, col="blue")
matlines(x.grid$x, se.bands, lwd =1, col="blue", lty =3)


library(mgcv)
model_gam=gam(average ~ s(numratings,bs='cr') + s(averageweight,bs='cr') + cluster + s(suggested_num_players,bs='cr') + s(playingtime, bs='cr'), data = games)
summary(model_gam)

hist(model_gam$residuals)
qqnorm(model_gam$residuals)
shapiro.test(model_gam$residuals[sample(1:3000)])

plot(model_gam)

model_gam_reduced=gam(average ~ s(averageweight,bs='cr') + cluster, data = games)
summary(model_gam_reduced)

anova(model_gam_reduced,model_gam, test = "F")


fit_MCD <- covMcd(x = games[,c('playingtime', 'suggested_num_players')], alpha = .99, nsamp = "best")
fit_MCD
colMeans(games[,c('playingtime', 'suggested_num_players')])

ind_best_subset <- fit_MCD$best
N <- nrow(games[,c('playingtime', 'suggested_num_players')])
p <- ncol(games[,c('playingtime', 'suggested_num_players')])
plot(games[,c('playingtime', 'suggested_num_players')], col=ifelse(1:N%in%ind_best_subset,"black","red"),pch=19)

#plot(fit_MCD,classic=TRUE)

games_best <- games[ind_best_subset,]
model_gam_best=gam(average ~ s(averageweight,bs='cr') + cluster + s(suggested_num_players,bs='cr') + s(playingtime, bs='cr'), data = games_best)
summary(model_gam_best)
plot(games_best[,c('playingtime', 'averageweight')],pch=19)
