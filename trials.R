# first set working directory where data.RDS is stored

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

games <- readRDS("data.RDS")

glimpse(games[1:30])
summary(games[1:30])

dim(games)
dim(games[games$year > 1990, ]) 
# maybe we can discard games before 1990 
# also because they are not wanted 
plot(games$year, games$wanting)
plot(games$year[games$wanting > 500], games$wanting[games$wanting > 500])


#### Interesting plots ####
ggpairs(games[,c('wishing','wanting','average','averageweight')])
# wanting and average are exponentially correlated but it's pretty obvious (wanting = exp(average))

boxplot(games$average ~ I(games$minage > 10))

boxplot(games$averageweight ~ I(games$wishing > 100)) # either too complex or too simple games are less desired

boxplot(games$averageweight ~ I(games$wanting > 50))

plot(games$minage, games$wanting)

plot(games$playingtime, games$wanting) # outliers can be detected with bagplot
bagplot <- bagplot(games$playingtime, games$wanting) 
# outliers wrt wanting -> important, might be very succcesfull game 
# outliers wrt playingtime -> can be discarded 
plot(games$playingtime[games$playingtime < 1000], games$wanting[games$playingtime < 1000])
bagplot <- bagplot(games$playingtime[games$playingtime < 1000], games$wanting[games$playingtime < 1000]) 
# in our analysis the most wanted games are important, we cannot discard them, bagplot is not so useful for outliers detection here 



#### permutational anova  wanting ~  2 <avg_weight < 4 ####


boxplot(games$wanting ~ I(games$averageweight < 4 & games$averageweight > 3)) 
length(which(games$averageweight < 4 & games$averageweight > 3)) # 328 
B = 1000
seed = 26111992
fit <- aov(games$wanting ~ I(games$averageweight < 4 & games$averageweight > 3))
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(games$wanting)
for(i in 1:B){
  perm <- sample(1:n)
  want_perm <- games$wanting[perm]
  fit_p <- aov( want_perm ~ I(games$averageweight < 4 & games$averageweight > 3) )
  T_stat[i] <- summary(fit_p)[[1]][1,4]
  
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val # as expected the difference is significant 


#### linear model for average weight  ####

## we saw that the games with an averageweight between 2 and 4 are more wanted
## than other ones (too complex or too simple ones)
## let's see if we can characterise the averageweight using other indicators 



with(games, plot( playingtime, averageweight))
# games with playingtime greater than 10000 can be considered outliers
# with(games, plot( playingtime, wanting))
# their wanting is low -> costumers are not looking for them

# let's consider games with a playingtime greater than one day 
out <- which(games$playingtime > 1440)

games$year[out] # they are not old
games$name[out]
length(which(games$Wargame[out] == 1))/length(out) # almost all wargames 
plot(games$playingtime,games$average, col = factor(games$playingtime > 10000))
plot( games$playingtime, games$numratings, col = factor(games$playingtime > 10000)) 
# they have high average but they have less reviews 

# let's keep them inside 

par(mfrow= c(1,3))
with(games, plot( minage, averageweight))
with(games, plot( numweights, averageweight))
with(games, plot( minplaytime, averageweight))
with(games, plot( year, averageweight)) # considering a company that wants to create a succesful game, this is not an covariate 
with(games, plot( minplayers, averageweight))
with(games, plot( maxplayers, averageweight)) # minplayers seems to be more informative 
# I can take out the ones with not available minage or minplayers or playingtime
# i.e. when these covariates are equal to zero 
# (I checked on boardgamegeek, they don't have a value provided for that category) 
par(mfrow= c(1,1))
ind <- which(games$minage == 0 | games$playingtime == 0 | games$minplayers == 0) # 1753 games 
y <- games$averageweight[-ind]
x1 <- games$minage[-ind]
x2 <- games$minplayers[-ind]
x3 <- games$playingtime[-ind]
gam_ssplines = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr'))  
summary(gam_ssplines) # 60% R^2 adjusted with these three covariates (with minage = 0 is 58%)


hist(gam_ssplines$residuals)
qqnorm(gam_ssplines$residuals)
qqline(gam_ssplines$residuals)
plot(gam_ssplines)

predict(gam_ssplines, data.frame(x1 = 2, x2 = 10, x3 = 18000 ))






#### Working on categories (WIP) ####

which(colnames(games) == 'Childrens.Game' | colnames(games) == 'Zombies')
first_cat <- 31
last_cat <- 100
two_way_table <- matrix(nrow = last_cat - first_cat + 1, ncol =last_cat - first_cat +1 )
dist_mat <- matrix(nrow = last_cat - first_cat + 1, ncol = last_cat - first_cat + 1)
for (i in first_cat:last_cat){
  for(j in first_cat:last_cat){
    cat_rows <- which(games[,i] == 1)
    
    common <- length(which(games[cat_rows,j] == 1))
    two_way_table[i - first_cat + 1 ,j - first_cat + 1 ]  <- common
    if (common == 0){
      dist_mat[i-first_cat + 1,j-first_cat + 1] <- 2
    }else{
      dist_mat[i-first_cat+ 1,j-first_cat+ 1] <- 1/common
    }
  }
}

dist_mat <- as.dist(dist_mat)
# rownames(dist_mat) <- colnames(games[,first_cat : last_cat])
colnames(two_way_table) <- rownames(two_way_table) <- colnames(games[, first_cat: last_cat])
dendo <- hclust(dist_mat, method='complete')
plot(dendo, main='complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
clusters <- rect.hclust(dendo, k=25) # reduce categories from 70 to 25 

### let's consider the three most common categories 
most_popular_categories <- names(sort(diag(two_way_table), decreasing = T)[1:3])
two_way_table[most_popular_categories, most_popular_categories]
# not lot of games belongs to these category simultaneously 

# let's see how the average weight changes between these categories
ind1 <- which(games[,most_popular_categories[1]] == 1)
hist(games$averageweight[ind1])

ind2 <- which(games[,most_popular_categories[2]] == 1)
hist(games$averageweight[ind2])

ind3 <- which(games[,most_popular_categories[3]] == 1)
hist(games$averageweight[ind3])
# I considered the average weight and not the wanting because the wanting 
# is not so significantly different. Thus, we can consider the average weight and 
# from that consider the wanting (based on the anova test done above)




#### Possible approach: consider only significant ratings ####


ind_1 <- which(games$numcomments/games$numratings < 0.2)
hist(games$average)
hist(games$average[-ind_1])
hist(games$average[ind_1] )
length(ind_1) #  not so many if compared to nrows

# linear model of before 
ind_2 <- which(games$minage == 0 | games$playingtime == 0 | games$minplayers == 0) # 1753 games 
ind <- c(ind_1, ind_2)
y <- games$averageweight[-ind]
x1 <- games$minage[-ind]
x2 <- games$minplayers[-ind]
x3 <- games$playingtime[-ind]
gam_ssplines = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr'))  
summary(gam_ssplines) # 60% R^2 adjusted 
















#### most wanted games ####
### (wishing and wanting have a correlation of almost 1 )
coll_wanting <- NULL
for(i in 31:100){
  ind <- games[,i] == 1
  coll_wanting <- c(coll_wanting, sum(games[ind,'wanting']))
}
df <- data.frame(coll_wanting)
rownames(df) <- colnames(games)[31:100] 
# df contains wanting per category 
most_wanted_category <- rownames(df)[which(df$coll_wanting == max(df$coll_wanting))] 
# card games are most wanted 




#### permutational anova  average_weight ~ wanting > 50 ####

summary(games$wishing)
summary(games$wanting)

length(which(games$wanting > 50)) # 17% 
length(which(games$wishing > 100))

boxplot(games$averageweight ~ I(games$wishing > 100)) # either too complex or too simple games are less desired
boxplot(games$averageweight ~ I(games$wanting > 50))
hist(games$averageweight[games$wanting > 50])
hist(games$averageweight[games$wanting < 50])


B = 1000
seed = 26111992
fit <- aov(games$averageweight ~ I(games$wanting > 50))
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(games$averageweight)
for(i in 1:B){
  perm <- sample(1:n)
  avgw_perm <- games$averageweight[perm]
  fit_p <- aov(avgw_perm ~ I(games$wanting > 50) )
  T_stat[i] <- summary(fit_p)[[1]][1,4]

}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val # as expected the difference is significant 


#### permutational anova average_weight ~ wishing > 100 ####
B = 1000
seed = 26111992
fit <- aov(games$averageweight ~ I(games$wishing > 100))
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(games$averageweight)
for(i in 1:B){
  perm <- sample(1:n)
  avgw_perm <- games$averageweight[perm]
  fit_p <- aov(avgw_perm ~ I(games$wishing > 100) )
  T_stat[i] <- summary(fit_p)[[1]][1,4]
  
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val # as expected the difference is significant 

#### permutational anova wanting ~ minage > 10 ####
# took 10 because is the mean of minage

boxplot(games$wanting ~ I(games$minage > 10))

B = 1000
seed = 26111992
fit <- aov(games$wanting ~ I(games$minage > 10))
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(games$wanting)
for(i in 1:B){
  perm <- sample(1:n)
  want_perm <- games$wanting[perm]
  fit_p <- aov(want_perm ~ I(games$minage > 10) )
  T_stat[i] <- summary(fit_p)[[1]][1,4]
  
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val # difference is significant 
# you can do it also with "average" -> still difference








