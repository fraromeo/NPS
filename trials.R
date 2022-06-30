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
# wanting and average are exponentially correlated but it's pretty obvious (wanting = exp(average))


boxplot(games$average ~ I(games$minage > 10))

boxplot(games$averageweight ~ I(games$wishing > 1000)) # either too complex or too simple games are less desired

boxplot(games$averageweight ~ I(games$wanting > 1000))

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

length(which(games$wanting > 50)) # 17% 
length(which(games$wishing > 100))

boxplot(games$averageweight ~ I(games$wishing > 100)) # either too complex or too simple games are less desired
boxplot(games$averageweight ~ I(games$wanting > 50))
hist(games$averageweight[games$wanting > 50])
hist(games$averageweight[games$wanting < 50])

## permutational anova to confirm difference between two groups
## average_weight ~ wanting > 1000
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


## permutational anova to confirm difference between two groups
## average_weight ~ wishing > 100
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
# you can do it also with "average" -> still difference


### linear models: let's start with a univariate nonparametric model wanting ~ playingtime

with(games, plot(playingtime, wanting))
# games with playingtime greater than 10000 can be considered outliers
# their wanting is low -> costumers are not looking for them
# let's see their characteristic, also compared to the others 
out <- which(games$playingtime > 10000)

games$year[out] # they are not old
games$name[out]
plot(games$playingtime,games$average, col = factor(games$playingtime > 10000))
plot( games$playingtime, games$numratings, col = factor(games$playingtime > 10000)) 
# they have high average but they have less reviews 

# let's do a linear model using cubic splines, for now we will not 
# take out the possible outliers

knots_pers <- c(seq(0,100, by = 10),150,seq(200, 1000, by = 200))
model_cubic_splines <-
  lm(wanting ~ bs(playingtime, degree = 3, knots = knots_pers), data = games) 

new_data <-
  with(games, data.frame(
    playingtime = seq(range(playingtime)[1], range(playingtime)[2], by = 2)
  ))
preds = predict(model_cubic_splines, new_data,se=T)
se.bands = cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

with(games, plot(playingtime , wanting ,xlim=range(new_data$playingtime) ,cex =.5, col =" darkgrey " ))
lines(new_data$playingtime,preds$fit ,lwd =2, col =" blue")
matlines(new_data$playingtime, se.bands ,lwd =1, col =" blue",lty =3)

ind <- games$playingtime < 1000
plot(games$playingtime[ind], games$wanting[ind], cex =.5, col =" darkgrey " )
ind2 <- new_data$playingtime < 1000
lines(new_data$playingtime[ind2],preds$fit[ind2] ,lwd =2, col =" blue")

## let's try to use a more local approach -> local averaging with gaussian kernel

want_loc = npreg(wanting ~ playingtime,
              ckertype = 'epanechnikov',
              bws = 5, # bandwidth
              data = games)

playingtime_newdata=data.frame(playingtime=with(games, seq(range(playingtime)[1],range(playingtime)[2],by=0.5)))
preds=predict(want_loc,newdata=playingtime_newdata,se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit) # we use standard error but we can maybe use something else 
with(
  games,
  plot(
    playingtime ,
    wanting ,
    xlim = range(playingtime_newdata$playingtime) ,
    cex = .5,
    col = " darkgrey ",
    main = 'Local Averaging - bws20 - Gaussian kernel'
  )
)
lines(playingtime_newdata$playingtime,preds$fit ,lwd =2, col =" blue")
matlines(playingtime_newdata$playingtime,se.bands ,lwd =1, col =" blue",lty =3)

ind <- games$playingtime < 1000
plot(games$playingtime[ind], games$wanting[ind], cex =.5, col =" darkgrey " )
ind2 <- playingtime_newdata$playingtime < 1000
lines(playingtime_newdata$playingtime[ind2],preds$fit[ind2] ,lwd =2, col =" blue")
matlines(playingtime_newdata$playingtime[ind2],se.bands[ind2,] ,lwd =1, col =" blue",lty =3)
# not what we want, there are too many data with a very low wanting 
length(which(games$wanting < 300))
# playingtime can not be used alone, is not so significant


## let's try to use a more complex linear model -> gam 

with(games, scatterplotMatrix(data.frame(average, playingtime, averageweight, minage)))
games_exclude <- which(games$minage == 0 | (games$numcomments/games$numratings < 0.2))
df <- games[-games_exclude,]
model_gam = gam(average ~ averageweight + s(minage, bs = 'cr') + s(playingtime, bs = 'cr'), data=df)
summary(model_gam)

model_gam2 = gam(average ~  s(wanting, bs = 'cr') + averageweight + s(minage, bs = 'cr'), data=df) # put wanting does not really make sense 
summary(model_gam2)



