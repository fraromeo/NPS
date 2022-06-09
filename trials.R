# first set working directory where data.RDS is stored

library(tidyverse)
library(tidytext)
library(caret)
library(cowplot)
library(dplyr)
library(GGally)
library(DepthProc)
library(aplpack)

games <- readRDS("data.RDS")

glimpse(games)
summary(games)

dim(games)
dim(games[games$year > 1990, ]) 
# maybe we can discard games before 1990 
# also because they are not wanted 
plot(games$year, games$wanting)
plot(games$year[games$wanting > 500], games$wanting[games$wanting > 500])


### Interesting plots 
ggpairs(games[,c('wishing','wanting','average','averageweight')])

boxplot(games$average ~ I(games$minage > 10))

boxplot(games$averageweight ~ I(games$wishing > 1000)) # either too complex or too simple games are less desired

boxplot(games$averageweight ~ I(games$wanting > 1000))

boxplot(games$average ~ I(games$minage > 10))

plot(games$minage, games$wanting)

plot(games$playingtime, games$wanting) # outliers can be detected with bagplot
bagplot <- bagplot(games$playingtime, games$wanting) 
# outliers wrt wanting -> important, might be very succcesfull game 
# outliers wrt playingtime -> can be discarded 
plot(games$playingtime[games$playingtime < 1000], games$wanting[games$playingtime < 1000])
bagplot <- bagplot(games$playingtime[games$playingtime < 1000], games$wanting[games$playingtime < 1000]) 
# in our analysis the most wanted games are important, we cannot discard them, bagplot is not so useful for outliers detection here 


#### analysis of most wished/wanted games (wishing and wanting have a correlation of almost 1 )
coll_wanting <- NULL
for(i in 31:100){
  ind <- games[,i] == 1
  coll_wanting <- c(coll_wanting, sum(games[ind,'wanting']))
}
df <- data.frame(coll_wanting)
rownames(df) <- colnames(games)[31:100] 
# df contains wanting per category 
most_wanted_category <- rownames(df)[which(df$coll_wanting == max(df$coll_wanting))]

summary(games$wishing)
summary(games$wanting)
# the variance is very high, the mean is not a reliable quantity 
length(which(games$wanting > 1000)) # only few -> very succesfull games 
length(which(games$wishing > 1000))

boxplot(games$averageweight ~ I(games$wishing > 1000)) # either too complex or too simple games are less desired
boxplot(games$averageweight ~ I(games$wanting > 1000))
hist(games$averageweight[games$wanting > 1000])
hist(games$averageweight[games$wanting < 1000])

## permutational anova to confirm difference between two groups
## average_weight ~ wanting > 1000
B = 1000
seed = 26111992
fit <- aov(games$averageweight ~ I(games$wanting > 1000))
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(games$averageweight)
for(i in 1:B){
  perm <- sample(1:n)
  avgw_perm <- games$averageweight[perm]
  fit_p <- aov(avgw_perm ~ I(games$wanting > 1000) )
  T_stat[i] <- summary(fit_p)[[1]][1,4]

}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val # as expected the difference is significant 

## permutational anova to confirm difference between two groups
## wanting ~ minage > 10
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

# you can do it also with average -> still difference











