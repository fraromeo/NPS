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


### dataset 
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

glimpse(games[1:22])
summary(games[1:22])

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
with(games, plot(suggested_num_players, averageweight))
# I can take out the ones with not available minage or minplayers or playingtime
# i.e. when these covariates are equal to zero 
# (I checked on boardgamegeek, they don't have a value provided for that category) 
par(mfrow= c(1,1))
ind <- which(games$minage == 0 | games$suggested_num_players == -1 | games$playingtime == 0 ) # 1753 games 
y <- games$averageweight[-ind]
x1 <- games$suggested_num_players[-ind]
x3 <- games$playingtime[-ind]
x2 <- games$minage[-ind]
gam_ssplines = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr'))  
summary(gam_ssplines) # 62% R^2 adjusted with these three covariates


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

#### Anova on categories after clustering ####

which(colnames(games) == 'Childrens.Game' | colnames(games) == 'Zombies')
first_cat <- 23
last_cat <- 61
two_way_table <- matrix(nrow = last_cat - first_cat + 1, ncol =last_cat - first_cat +1 )
dist_mat <- matrix(nrow = last_cat - first_cat + 1, ncol = last_cat - first_cat + 1)
for (i in first_cat:last_cat){
  for(j in first_cat:last_cat){
    cat_rows <- which(games[,i] == 1)
    
    common <- length(which(games[cat_rows,j] == 1))
    two_way_table[i - first_cat + 1 ,j - first_cat + 1 ]  <- common
    dist_mat[i-first_cat+ 1,j-first_cat+ 1] <- 1/(common+1)
  }
}


# rownames(dist_mat) <- colnames(games[,first_cat : last_cat])
colnames(two_way_table) <- rownames(two_way_table) <- colnames(games[, first_cat: last_cat])
colnames(dist_mat) <- rownames(dist_mat) <- colnames(games[, first_cat: last_cat])
dendo <- hclust(as.dist(dist_mat), method='complete')
plot(dendo, main='complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
clusters <- rect.hclust(dendo, k=9) # reduce categories from 70 to 25 
cluster <- cutree(dendo, k=9)

cat_aov <- NULL
for(i in 1:8){
  
  cat <- names(cluster[which(cluster == i)])
  occurs <- diag(two_way_table[cat,cat])
  cat_aov <- c(cat_aov,names(occurs[which(occurs == max(occurs))]))
} 
two_way_table[cat_aov,cat_aov]


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
    newfm <- reformulate(deparse(long_formula[[3]]),response = "y.perm.H0")
    T_stat[perm] <- summary.aov(aov(newfm))[[1]][to_test,4]
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

short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Party.Game + games$Print...Play
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Party.Game + games$Print...Play + games$Puzzle
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 8) # pval 0.075 games$Puzzle not significant 

short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Party.Game
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Party.Game + games$Print...Play
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 7) #  games$Print...Play significant 

short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Print...Play
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Party.Game + games$Print...Play
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 6) #  games$Party.Game  significant 

short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + + games$Party.Game + games$Print...Play
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Party.Game + games$Print...Play
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 5) #  games$Economic  significant 

short_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice +  games$Economic + games$Party.Game + games$Print...Play
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Party.Game + games$Print...Play
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 4) #  games$Medieval  significant 

short_formula <- games$wanting ~ games$Card.Game + games$Wargame  + games$Medieval +  games$Economic + games$Party.Game + games$Print...Play
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Dice + games$Medieval + games$Economic + games$Party.Game + games$Print...Play
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 3) #  pval 0.021 games$Dice  not significant 

short_formula <- games$wanting ~ games$Card.Game + games$Medieval +  games$Economic + games$Party.Game + games$Print...Play
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Medieval + games$Economic + games$Party.Game + games$Print...Play
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 2) # games$Wargame significant 


short_formula <- games$wanting ~  games$Wargame + games$Medieval +  games$Economic + games$Party.Game + games$Print...Play
long_formula <- games$wanting ~ games$Card.Game + games$Wargame + games$Medieval + games$Economic + games$Party.Game + games$Print...Play
pval <- perm_anova_twoway_interaction(games$wanting, short_formula, long_formula, 1) # games$Card.Game significant 

final_model <- aov(long_formula)
final_model$coefficient

## economic is the type that increases the wanting the most. It has a pos coeff, together with medieval 
## the others have negative coefficients 

#### anova for wanting/average/averageweight for three most popular categories ####

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


boxplot(scale(games$wanting) ~ games$Card.Game)
boxplot(games$wanting ~ games$Wargame)
boxplot(games$wanting ~ games$Fantasy)

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
    newfm <- reformulate(deparse(long_formula[[3]]),response = "y.perm.H0")
    T_stat[perm] <- summary.aov(aov(newfm))[[1]][to_test,4]
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


#### don't run 
explainer <- shapr(df_train,  gam_ssplines)
p <- mean(y)
df_test <- data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind])
explanation <- explain(df_test,approach = "empirical",explainer = explainer, prediction_zero = p )
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6,10))













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



#### permutational anova  wanting + owned ~   low_avgw < avg_weight < high_avgw ####
outcome <- games$wanting + games$owned 
low_avgw = 2.5
high_avgw = 3

boxplot(outcome  ~ I(games$averageweight < high_avgw & games$averageweight > low_avgw)) 

B = 1000
seed = 2022

fit <- aov( outcome ~  I(games$averageweight < high_avgw & games$averageweight > low_avgw))
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(outcome)
for(i in 1:B){
  perm <- sample(1:n)
  outcome_perm <- outcome[perm]
  fit_p <- aov( outcome_perm ~ I(games$averageweight < high_avgw & games$averageweight > low_avgw) )
  T_stat[i] <- summary(fit_p)[[1]][1,4]
  
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val # difference between groups 

aggregate(outcome, list(I(games$averageweight < high_avgw & games$averageweight > low_avgw)), FUN = mean)

# groups with avgweight between 2.5 and 3 have higher wanting + owned on average 

















#### permutational anova  wanting ~  low_avgw < avg_weight < high_avgw & cluster ####

low_avgw = 2.5
high_avgw = 3
boxplot(games$wanting ~ I(games$averageweight < high_avgw & games$averageweight > low_avgw) + games$cluster) 

B = 1000

perm_anova_twoway_interaction(games$wanting, factor1 = I(games$averageweight < high_avgw & games$averageweight > low_avgw), 
                              factor2 = games$cluster, iter = B)
# interaction term is significant 
perm_anova_twoway_factor12(games$wanting, factor1 = I(games$averageweight < high_avgw & games$averageweight > low_avgw), 
                           factor2 = games$cluster, interac = TRUE, iter = B)
# games$cluster is not significant, the first factor is significant. 
# better to remain with the simpler model with only first factor 




#### permutational anova  wanting ~  low_avgw < avg_weight < high_avgw ####
low_avgw = 2.5
high_avgw = 3
boxplot(games$wanting ~ I(games$averageweight < high_avgw & games$averageweight > low_avgw)) 
length(which(games$averageweight < high_avgw & games$averageweight > low_avgw)) # 1587 
B = 1000
seed = 26111992
fit <- aov(games$wanting ~ I(games$averageweight < high_avgw & games$averageweight > low_avgw))
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(games$wanting)
for(i in 1:B){
  perm <- sample(1:n)
  want_perm <- games$wanting[perm]
  fit_p <- aov( want_perm ~ I(games$averageweight < high_avgw & games$averageweight > low_avgw) )
  T_stat[i] <- summary(fit_p)[[1]][1,4]
  
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val
# significative difference: from the boxplot we can see that games with complexity 
# between 2.5 and three are more wanted 


#### permutational anova  average ~  low_avgw < avg_weight < high_avgw & cluster ####

low_avgw = 2.5
high_avgw = 3
boxplot(games$average ~ I(games$averageweight < high_avgw & games$averageweight > low_avgw) + games$cluster) 

B = 1000

perm_anova_twoway_interaction(games$average, factor1 = I(games$averageweight < high_avgw & games$averageweight > low_avgw), 
                              factor2 = games$cluster, iter = B)
# interaction term is not significant 
perm_anova_twoway_factor12(games$average, factor1 = I(games$averageweight < high_avgw & games$averageweight > low_avgw), 
                           factor2 = games$cluster, interac = FALSE, iter = B)
# both terms are significant













#### permutational anova  averageweight ~   cluster ####
outcome <- games$averageweight
boxplot(outcome ~  games$cluster) 


B = 1000
seed = 2022

fit <- aov( outcome ~  games$cluster)
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(outcome)
for(i in 1:B){
  perm <- sample(1:n)
  outcome_perm <- outcome[perm]
  fit_p <- aov( outcome_perm ~ games$cluster )
  T_stat[i] <- summary(fit_p)[[1]][1,4]
  
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val # difference between groups 





#### permutational anova  average ~   cluster ####
outcome <- games$average
boxplot(outcome ~  games$category.cluster) 


B = 1000
seed = 2022

fit <- aov( outcome ~  games$category.cluster)
T0 <- summary(fit)[[1]][1,4]

T_stat <- numeric(B)
n <- length(outcome)
for(i in 1:B){
  perm <- sample(1:n)
  outcome_perm <- outcome[perm]
  fit_p <- aov( outcome_perm ~ games$category.cluster )
  T_stat[i] <- summary(fit_p)[[1]][1,4]
  
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val # difference between groups 







#### permutational anova multiple ways  average ~   cluster ####
outcome <- games$average
dummy_vars <- fastDummies::dummy_cols(games$cluster)
boxplot(outcome ~  games$cluster, col = unique(games$cluster + 1)) 

# remember that first col of dummy_vars is general data, is not a cluster, so i-th col is i-th -1 cluster
short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 6) # pval 0 6th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 5) # pval 0 5th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 4) # pval 0 4th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3]+ dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 3) # pval 0.78 3th cluster is not significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 2) # pval 0.814 2th cluster is not significant 

short_formula <- outcome ~  dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 1) # pval 0 1st cluster is significant 
## last group  
short_formula <- outcome ~  dummy_vars[,2] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] + dummy_vars[,8]
pval <- perm_anova_nway(outcome, short_formula, long_formula, 5) # pval 0.035, not significant (given that we did multiple tests)

long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7]

final_model <- aov(long_formula)
final_model$coefficient

## belonging to 4th and 5th group increases the average, 6th and 1st decreases it 







#### permutational anova multiple ways  average weight ~   cluster ####
outcome <- games$averageweight
dummy_vars <- fastDummies::dummy_cols(games$cluster)
boxplot(outcome ~  games$cluster, col = unique(dummy_vars[,1] +1)) 

# remember that first col of dummy_vars is general data, is not a cluster, so i-th col is i-th -1 cluster
short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 6) # pval 0 6th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 5) # pval 0 5th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 4) # pval 0 4th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3]+ dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 3) # pval 1 3th cluster is not significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 2) # pval 0 2th cluster is  significant 

short_formula <- outcome ~   dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 1) # pval 0 1st cluster is significant 
## last group  
short_formula <- outcome ~  dummy_vars[,2]+ dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2]+ dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] + dummy_vars[,8]
pval <- perm_anova_nway(outcome, short_formula, long_formula, 6) # pval 0.035, not significant (given that we did multiple tests)

long_formula <- outcome ~ dummy_vars[,2]+ dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7]+ dummy_vars[,8]

final_model <- aov(long_formula)
final_model$coefficient

## belonging to 2nd 4th and 5th group increases the average, 6th and 1st  and 7th decreases it 







#### permutational anova multiple ways  wanting + owned ~   cluster ####
outcome <- games$wanting + games$owned
dummy_vars <- fastDummies::dummy_cols(games$cluster)
boxplot(outcome ~  games$cluster, col = unique(dummy_vars[,1] +1)) 

# remember that first col of dummy_vars is general data, is not a cluster, so i-th col is i-th -1 cluster
short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 6) # pval 0.018 6th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 5) # pval 0.71 5th cluster is not significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5]  + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 4) # pval 0 4th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5]  +dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5]  + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 3) # pval 0.93 3th cluster is not significant 

short_formula <- outcome ~ dummy_vars[,2]  + dummy_vars[,5]  +dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3]  + dummy_vars[,5]  + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 2) # pval 0 2th cluster is  significant 

short_formula <- outcome ~ dummy_vars[,3]  + dummy_vars[,5]  +dummy_vars[,7] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3]  + dummy_vars[,5]  + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 1) # pval 0.31 1st cluster is not significant 
## last group  
short_formula <- outcome ~ dummy_vars[,3]  + dummy_vars[,5]  + dummy_vars[,7] 
long_formula <- outcome ~  dummy_vars[,3]  + dummy_vars[,5]  + dummy_vars[,7] +dummy_vars[,8]
pval <- perm_anova_nway(outcome, short_formula, long_formula, 4) # pval 0.001 significant 

long_formula <- outcome ~ dummy_vars[,3]  + dummy_vars[,5]  + dummy_vars[,7] +dummy_vars[,8]

final_model <- aov(long_formula)
final_model$coefficient

## belonging to 2nd 4th group increases the wanting + owned, 6th and 7th decreases it 











