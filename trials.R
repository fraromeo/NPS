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
library(kernlab)

games <- readRDS("data.RDS")

glimpse(games[1:30])
summary(games[1:30])

dim(games)
dim(games[games$year > 1990, ]) 



#### General stuff ####


# maybe we can discard games before 1990 
# also because they are not wanted 
plot(games$year, games$wanting)
plot(games$year[games$wanting > 500], games$wanting[games$wanting > 500])


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



#### permutational anova  wanting ~  3 < avg_weight < 4 ####


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

## we saw that the games with an averageweight between 3 and 4 are more wanted
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
with(games, plot( year, averageweight)) # considering a company that wants to create a succesful game, this is not a covariate 
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






#### relationship between average and averageweight and linear models for average ####
ggplot(games[,c('average','averageweight')], aes(x = averageweight, y = average) ) + 
  geom_violin()
plot(games$averageweight, games$average)
ind <- which(games$year > 2017) # more recent games
y <- games$average[ind]
x <- games$averageweight[ind]
local_model = npreg(y ~ x,
                    ckertype = 'epanechnikov',
                    bws = 0.5) # set the bandwidth



x.grid=data.frame(x=seq(range(x)[1],range(x)[2],by=0.1))
preds=predict(local_model, newdata=x.grid, se=T)
se.bands=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
plot(x, y, xlim = range(x.grid$x), cex=.5, col="darkgrey")
lines(x.grid$x, preds$fit, lwd =2, col="blue")
matlines(x.grid$x, se.bands, lwd =1, col="blue", lty =3)

## let's try a Gaussian Process
## data are too much so for this I will only consider games with a 
## year > 2017 
y <- games$average
x <- games$averageweight
ind <- which(games$year > 2017)
x <- x[ind]
y <- y[ind]
gp <- gausspr(x,y, variance.model = TRUE)

xtest <- sort(x)
preds <- predict(gp, sort(x))
# preds.se <- predict(gp, sort(x),  type='sdeviation' )

plot(x,y, pch = 20)
lines(sort(x), preds, lwd =3, col = 'blue')
#lines(sort(x),
#      preds+2*preds.se,
#      col="blue", lty = 3)
#lines(sort(x),
#      preds-2*preds.se,
#      col="blue", lty = 3)


## another linear model 

ind1 <- which(games$minage == 0 | games$playingtime == 0 | games$minplayers == 0 | games$year < 2017)

y <- games$average[-ind]
x4 <- games$averageweight[-ind]
x1 <- games$minage[-ind]
x2 <- games$minplayers[-ind]
x3 <- games$playingtime[-ind]
gam_ssplines_avg = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + s(x4, bs='cr'))  
summary(gam_ssplines_avg) # only 32 % 


#### Linear regression for the wanting ####
boxplot(games$wanting ~ games$Zombies)

boxplot(games$wanting ~ games$Card.Game)

boxplot(games$minage ~ I(games$wanting > 1000))
plot(games$minage, games$wanting, col = 2*I(games$wanting > 1000)+1)

my_col <- c('green', 'red')
ind <- games$wanting > 50
pairs(games[,c('averageweight', 'average', 'playingtime', 'minage','minplayers', 'numcomments')], 
      pch = c(1,19), cex = 0.5, col = my_col[ind + 1] )

hist(games$wanting, breaks = 100)
plot(games$year, games$wanting)
y <- games$wanting
x1 <- games$numcomments
x2 <- games$minage
x3 <- games$year
x4 <- games$minplayers
x5 <- games$playingtime

model1 <- gam(y ~ s(x1, bs = 'cr'))
summary(model1) # 65% R^2 adj with all covariates, without numcomments it drops at 7%, only with numcomments -> 61 %  
plot(x1, model1$fitted.values, pch = 19)
points(x1, y, col = 'green')

model2 <- gam(x1 ~ s(x2, bs = 'cr') + s(x3, bs = 'cr') + s(x4, bs = 'cr') + s(x5, bs = 'cr'))
summary(model2)

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
clusters <- rect.hclust(dendo, k=20) # reduce categories from 70 to 25 




### let's consider the three most common categories 
most_popular_categories <- names(sort(diag(two_way_table), decreasing = T)[1:3])
two_way_table[most_popular_categories, most_popular_categories]
# half of the games belongs to these three categories 
# not lot of games belongs to these category simultaneously 

# let's see how the average weight changes between these categories
ind1 <- which(games[,most_popular_categories[1]] == 1)
hist(games$averageweight[ind1])

ind2 <- which(games[,most_popular_categories[2]] == 1)
hist(games$averageweight[ind2])

ind3 <- which(games[,most_popular_categories[3]] == 1)
hist(games$averageweight[ind3])
# I considered the average weight and not the wanting because the wanting 
# is not so significantly different.

## anova three way of most popular categories on wanting 

model_aov3 <- aov(games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy + games$Fantasy:games$Wargame)


boxplot(scale(games$wanting) ~ games$Card.Game)
boxplot(games$wanting ~ games$Wargame)
boxplot(games$wanting ~ games$Fantasy)
short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy + games$Fantasy:games$Wargame

perm_anova_twoway_interaction = function(outcome, short_formula, long_formula, to_test, iter=1e3){
  T0 <- summary.aov(aov(long_formula))[[1]][to_test,4]
  T_stat <- numeric(iter)
  n <- length(outcome)
  aov.H0 <- aov(short_formula)
  residuals.H0 <- aov.H0$residuals
  pb = progress::progress_bar$new(total = iter,
                                  format = " Processing [:bar] :percent eta: :eta")
  set.seed(2022)
  for(perm in 1:iter){
    permutation <- sample(1:n)
    residuals.H0 <- residuals.H0[permutation]
    y.perm.H0 <- aov.H0$fitted + residuals.H0
    T_stat[perm] <- summary.aov(aov(y.perm.H0 ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Fantasy))[[1]][to_test,4]
    pb$tick()
  }
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30) 
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat), xlim = range(c(T_stat,T0)))
  abline(v=T0,col=3,lwd=2)
  p_val <- sum(T_stat>=T0)/iter
  print(p_val)
  return(p_val) 
}

short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy + games$Fantasy:games$Wargame
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 6) # games$Fantasy:games$Wargame not significant 


short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame 
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy 
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 5) # pval 0.014 <- using a bonferroni correction (0.1/6 = 0.017) we can say games$Card.Game:games$Fantasy is significant 

short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Fantasy 
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy 
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 4) # pval 0.048 games$Card.Game:games$Wargame is not significant 

short_formula <- games$wanting ~ games$Card.Game + games$Wargame  + games$Card.Game:games$Fantasy 
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Card.Game:games$Fantasy 
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 3) # games$Fantasy significant 

short_formula <- games$wanting ~ games$Card.Game + games$Fantasy  + games$Card.Game:games$Fantasy 
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Card.Game:games$Fantasy 
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 2) # games$Wargame significant 

short_formula <- games$wanting ~games$Wargame + games$Fantasy  + games$Card.Game:games$Fantasy 
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Card.Game:games$Fantasy 
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 1) # games$Card.Game significant 

model_final <- aov(games$wanting ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Card.Game:games$Fantasy )
summary(model_final)
model_final$coefficients
### To conclude: 
### Card games -> negative coefficients -> being a card game decreases the wanting on average
### War games -> negative coefficients -> being a war games decreases the wanting on average
### Fantasy -> positive coefficients -> being a Fantasy game increases the wanting on average
### Interaction between card games and fantasy games -> negative coefficients -> being 
### both a card game and fantasy game decreases the wanting on average

## anova three way of most popular categories on average 


perm_anova_twoway_interaction = function(outcome, short_formula, long_formula, to_test, iter=1e3){
  T0 <- summary.aov(aov(long_formula))[[1]][to_test,4]
  T_stat <- numeric(iter)
  n <- length(outcome)
  aov.H0 <- aov(short_formula)
  residuals.H0 <- aov.H0$residuals
  pb = progress::progress_bar$new(total = iter,
                                  format = " Processing [:bar] :percent eta: :eta")
  set.seed(2022)
  for(perm in 1:iter){
    permutation <- sample(1:n)
    residuals.H0 <- residuals.H0[permutation]
    y.perm.H0 <- aov.H0$fitted + residuals.H0
    T_stat[perm] <- summary.aov(aov(y.perm.H0 ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame))[[1]][to_test,4]
    pb$tick()
  }
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30) 
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat), xlim = range(c(T_stat,T0)))
  abline(v=T0,col=3,lwd=2)
  p_val <- sum(T_stat>=T0)/iter
  print(p_val)
  return(p_val) 
}

short_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy
long_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy + games$Fantasy:games$Wargame
pval <- perm_anova_twoway_interaction(games$average, short_formula, long_formula, 6) # games$Fantasy:games$Wargame is  significant 


short_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Fantasy:games$Wargame
long_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy + games$Fantasy:games$Wargame
pval <- perm_anova_twoway_interaction(games$average, short_formula, long_formula, 5) # pval 0.036 <- using a bonferroni correction (0.1/6 = 0.017) we can say games$Card.Game:games$Fantasy is not significant 

short_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy + games$Fantasy:games$Wargame
long_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Fantasy:games$Wargame
pval <- perm_anova_twoway_interaction(games$average, short_formula, long_formula, 4) # pval 0.054 games$Card.Game:games$Wargame is not significant 

short_formula <- games$average ~ games$Card.Game + games$Wargame  + games$Fantasy:games$Wargame 
long_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame 
pval <- perm_anova_twoway_interaction(games$average, short_formula, long_formula, 3) # games$Fantasy significant 

short_formula <- games$average ~ games$Card.Game + games$Fantasy  + games$Fantasy:games$Wargame 
long_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame 
pval <- perm_anova_twoway_interaction(games$average, short_formula, long_formula, 2) # games$Wargame significant 

short_formula <- games$average ~games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame 
long_formula <- games$average ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame 
pval <- perm_anova_twoway_interaction(games$average, short_formula, long_formula, 1) # games$Card.Game significant 

model_final <- aov(games$average ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame )
summary(model_final)
model_final$coefficients
### To conclude: 
### Card games -> negative coefficients -> being a card game decreases the average grade on average
### War games -> positive coefficients -> being a war games increases the average grade on average
### Fantasy -> positive coefficients -> being a Fantasy game increases the average grade on average
### Interaction between war games and fantasy games -> negative coefficients -> being 
### both a card game and fantasy game decreases the average grade on average


## anova three way of most popular categories on average 


perm_anova_twoway_interaction = function(outcome, short_formula, long_formula, to_test, iter=1e3){
  T0 <- summary.aov(aov(long_formula))[[1]][to_test,4]
  T_stat <- numeric(iter)
  n <- length(outcome)
  aov.H0 <- aov(short_formula)
  residuals.H0 <- aov.H0$residuals
  pb = progress::progress_bar$new(total = iter,
                                  format = " Processing [:bar] :percent eta: :eta")
  set.seed(2022)
  for(perm in 1:iter){
    permutation <- sample(1:n)
    residuals.H0 <- residuals.H0[permutation]
    y.perm.H0 <- aov.H0$fitted + residuals.H0
    T_stat[perm] <- summary.aov(aov(y.perm.H0 ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy + games$Fantasy:games$Wargame))[[1]][to_test,4]
    pb$tick()
  }
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30) 
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat), xlim = range(c(T_stat,T0)))
  abline(v=T0,col=3,lwd=2)
  p_val <- sum(T_stat>=T0)/iter
  print(p_val)
  return(p_val) 
}

short_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy
long_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy + games$Fantasy:games$Wargame
pval <- perm_anova_twoway_interaction(games$averageweight, short_formula, long_formula, 6) # games$Fantasy:games$Wargame is significant 


short_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Fantasy:games$Wargame
long_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Card.Game:games$Fantasy + games$Fantasy:games$Wargame
pval <- perm_anova_twoway_interaction(games$averageweight, short_formula, long_formula, 5) # pval 0.05 <- using a bonferroni correction (0.1/6 = 0.017) we can say games$Card.Game:games$Fantasy is not significant 

short_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy + games$Fantasy:games$Wargame
long_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy + games$Card.Game:games$Wargame + games$Fantasy:games$Wargame
pval <- perm_anova_twoway_interaction(games$averageweight, short_formula, long_formula, 4) # pval 0.0 games$Card.Game:games$Wargame is significant 

short_formula <- games$averageweight ~ games$Card.Game + games$Wargame  + games$Fantasy:games$Wargame + games$Card.Game:games$Wargame
long_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame + games$Card.Game:games$Wargame
pval <- perm_anova_twoway_interaction(games$averageweight, short_formula, long_formula, 3) # games$Fantasy significant 

short_formula <- games$averageweight ~ games$Card.Game + games$Fantasy  + games$Fantasy:games$Wargame + games$Card.Game:games$Wargame
long_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame + games$Card.Game:games$Wargame
pval <- perm_anova_twoway_interaction(games$averageweight, short_formula, long_formula, 2) # games$Wargame significant 

short_formula <- games$averageweight ~games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame + games$Card.Game:games$Wargame
long_formula <- games$averageweight ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame + games$Card.Game:games$Wargame
pval <- perm_anova_twoway_interaction(games$averageweight, short_formula, long_formula, 1) # games$Card.Game significant 

model_final <- aov(games$average ~ games$Card.Game + games$Wargame + games$Fantasy  + games$Fantasy:games$Wargame + games$Card.Game:games$Wargame)
summary(model_final)
model_final$coefficients
### To conclude: 
### Card games -> negative coefficients -> being a card game decreases the complexity on average
### War games -> positive coefficients (coeff is larger than fantasy games)-> being a war games increases the complexity on average
### Fantasy -> positive coefficients -> being a Fantasy game increases the complexity on average
### Interaction between war games and fantasy games -> negative coefficients -> being 
### both a card game and fantasy game decreases the complexity on average
### Interaction between war games and card games -> negative coefficients -> being 
### both a card game and fantasy game decreases the complexity on average




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








