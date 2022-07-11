rm(list=ls())
games <- read.csv('clusterized_games3.csv')

library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(np)
library(rgl)
library(xgboost)
library(GGally)
library(dplyr)
library(tidyverse)
library(conformalInference)
library(dbscan)
#### functions ####
# for shapley value 
source("shap.R")

# for clusters
compute.cooccurrence <- function(X) {
  X <- as.matrix(X)
  out <- crossprod(X)  # Same as: t(X) %*% X
  #diag(out) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)
  out
}
most_common <- function(clust, num_cat, cat = T){
  # num_cat = number of most common categories needed (e.g. three most common cat ...)
  category.indices <-  24:62
  if (cat){
    ind_cl <- which(games$category.cluster.kmed == clust)
  }else{
    ind_cl <- which(games$mechanic.cluster.kmed == clust)
  }
  
  games_no_fac <- sapply(games[ind_cl,category.indices], function(x) as.numeric(as.character(x)))
  coocc <- diag(compute.cooccurrence(games_no_fac))
  print(names(sort(coocc, decreasing = T))[1:num_cat])
}


# others 
perm_anova_twoway_interaction = function(outcome,factor1,factor2,iter=1e3){
  T0 <- summary.aov(aov(outcome ~ factor1 + factor2 + factor1:factor2))[[1]][3,4]
  T_stat <- numeric(iter)
  n <- length(outcome)
  aov.H0 <- aov(outcome ~ factor1 + factor2)
  residuals.H0 <- aov.H0$residuals
  pb = progress::progress_bar$new(total = iter,
                                  format = " Processing [:bar] :percent eta: :eta")
  set.seed(2022)
  for(perm in 1:iter){
    permutation <- sample(1:n)
    residuals.H0 <- residuals.H0[permutation]
    y.perm.H0 <- aov.H0$fitted + residuals.H0
    T_stat[perm] <- summary.aov(aov(y.perm.H0 ~ factor1 + factor2 + factor1:factor2))[[1]][3,4]
    pb$tick()
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30) 
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat), xlim = range(c(T_stat,T0)))
  abline(v=T0,col=3,lwd=2)
  p_val <- sum(T_stat>=T0)/iter
  # print(p_val)
  return(p_val) 
}

perm_anova_twoway_factor12 = function(outcome,factor1,factor2, interac = FALSE, iter=1e3){
  if (interac){
    full_mod <- outcome ~ factor1 + factor2 + factor1:factor2
    short_mod1 <- outcome ~ factor2 + factor1:factor2
    short_mod2 <- outcome ~ factor1 + factor1:factor2
  }else{
    full_mod <- outcome ~ factor1 + factor2 
    short_mod1 <- outcome ~ factor2 
    short_mod2 <- outcome ~ factor1
  }
    
    
  T0_1 <- summary.aov(aov(full_mod ))[[1]][1,4]
  T0_2 <- summary.aov(aov(full_mod ))[[1]][2,4]
  T_stat_1 <- T_stat_2 <- numeric(iter)
  n <- length(outcome)
  aov.H0_1 <- aov(short_mod1 )
  residuals.H0_1 <- aov.H0_1$residuals
  aov.H0_2 <- aov(short_mod2 )
  residuals.H0_2 <- aov.H0_2$residuals
  pb = progress::progress_bar$new(total = iter,
                                  format = " Processing [:bar] :percent eta: :eta")
  set.seed(2022)
  for(perm in 1:iter){
    permutation <- sample(1:n)
    y.perm.H0_1 <- aov.H0_1$fitted + residuals.H0_1[permutation]
    newfm <- reformulate(deparse(full_mod[[3]]), response = "y.perm.H0_1")
    T_stat_1[perm] <- summary.aov(aov(newfm))[[1]][1,4]
    y.perm.H0_2 <- aov.H0_2$fitted + residuals.H0_2[permutation]
    newfm <- reformulate(deparse(full_mod[[3]]), response = "y.perm.H0_2")
    T_stat_2[perm] <- summary.aov(aov(newfm))[[1]][2,4]
    pb$tick()
  }
  
  hist(T_stat_1,xlim=range(c(T_stat_1,T0_1)),breaks=30) 
  abline(v=T0_1,col=3,lwd=2)
  
  hist(T_stat_2,xlim=range(c(T_stat_2,T0_2)),breaks=30) 
  abline(v=T0_2,col=3,lwd=2)
  
  plot(ecdf(T_stat_1), xlim=range(c(T_stat_1,T0_1)))
  abline(v=T0_1,col=3,lwd=2)
  
  plot(ecdf(T_stat_2), xlim=range(c(T_stat_2,T0_2)))
  abline(v=T0_2,col=3,lwd=2)
  print(sum(T_stat_1>=T0_1)/iter)
  print(sum(T_stat_2>=T0_2)/iter)
}

perm_anova_nway = function(outcome, short_formula, long_formula, to_test, iter=1e3){
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

perm_anova_oneway = function(outcome,factor,iter=1e3){
  fit <- aov(outcome ~ factor)
  T0 <- summary(fit)[[1]][1,4]
  T_stat <- numeric(iter)
  n <- length(outcome)
  pb = progress::progress_bar$new(total = iter,
                                  format = " Processing [:bar] :percent eta: :eta")
  set.seed(2022)
  for(perm in 1:iter){
    permutation <- sample(1:n)
    outcome_perm <- outcome[permutation]
    fit_perm <- aov(outcome_perm ~ factor)
    T_stat[perm] <- summary(fit_perm)[[1]][1,4]
    pb$tick()
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30) 
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat), xlim=range(c(T_stat,T0)))
  abline(v=T0,col=3,lwd=2)
  p_val <- sum(T_stat>=T0)/iter
  print(p_val)
  return(p_val) 
}




#### characterization of groups ####
most_common(1,3) # three most common categories in first cluster 
most_common(2,3)
most_common(3,1)
most_common(4,1)
most_common(5,3)
most_common(6,3)



#### permutational anova  wanting + owned ~ mechanic + category ####
outcome <- games$wanting + games$owned 
factor1 <- games$category.cluster.kmed
factor2 <- games$mechanic.cluster.kmed
boxplot(outcome ~  factor1, col = unique(factor1 + 1 ), pch = 19, cex = 0.5, xlab = "clusters on categories", ylab = "appeal") 
boxplot(outcome ~  factor2, col = unique(factor2 + 1 ), pch = 19, cex = 0.5, xlab = "clusters on mechanics", ylab = "appeal")

B = 1000
seed = 2022

perm_anova_twoway_interaction(outcome, factor1 = factor1, 
                              factor2 = factor2, iter = B)
# interaction term is not significant 
perm_anova_twoway_factor12(outcome, factor1 = factor1, 
                           factor2 = factor2, interac = F, iter = B)
## neither of them is significant, as we can see from the boxplots above 
## also with the other clusters 
# factor1 <- games$mechanic.cluster
# factor2 <- games$category.cluster
# the result is the same 


#### permutational anova  average ~ mechanic + category ####
outcome <- games$average
factor1 <- games$category.cluster.kmed
factor2 <- games$mechanic.cluster.kmed
boxplot(outcome ~  factor1, col = unique(factor1 + 1 ), ylab = 'average rating', xlab = 'clusters of categories', pch = 19, cex = 0.5) 
boxplot(outcome ~  factor2, col = unique(factor2 + 1 ), ylab = 'average rating', xlab = 'clusters of mechanics', pch = 19, cex = 0.5)

B = 1000
seed = 2022

perm_anova_twoway_interaction(outcome, factor1 = factor1, 
                              factor2 = factor2, iter = B)
# interaction term is significant 
perm_anova_twoway_factor12(outcome, factor1 = factor1, 
                           factor2 = factor2, interac = T, iter = B)
# first group is significant, second is not 
# we can try to focus on the model with only the first factor, making an anova multiple 
# ways in which the i-th group are "belonging or not to the ith group"

#### permutational anova multiple ways  average ~ category ####

outcome <- games$average
dummy_vars <- fastDummies::dummy_cols(games$category.cluster.kmed)
boxplot(outcome ~  games$category.cluster.kmed, col = unique(games$category.cluster.kmed + 1)) 

# first col of dummy_vars is general data, is not a cluster, so i-th col is i-th -1 cluster
short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5]
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 5) # pval 0 5th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6]  
pval <- perm_anova_nway(outcome, short_formula, long_formula, 4) # pval 0 4th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6]  
pval <- perm_anova_nway(outcome, short_formula, long_formula, 3) # pval 1 3rd cluster is not significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,5] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 2) # pval 0.007 2nd   cluster is not significant 

short_formula <- outcome ~ dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6]  
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 1) # pval 0 1st cluster is significant 

## last group  
short_formula <- outcome ~  dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 5) # pval 0  significant

long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 

final_model <- aov(long_formula)
final_model$coefficient

## belonging to 4th group increases the average, 6th and 2nd and 1st decreases it 

most_common(4,3)

special_cat <- ifelse(games$Wargame == 1 | games$Ancient == 1 | games$World.War.II == 1, 1, 0)
length(which(special_cat == 1))
boxplot(games$average ~ special_cat, col = unique(special_cat + 2), pch = 19, cex = 0.5, ylab= "average rating", xlab = " War game, world war II, ancient")

perm_anova_oneway(games$average, special_cat) 


#### permutational anova  complexity ~ mechanic + category ####
outcome <- games$averageweight
factor1 <- games$category.cluster.kmed
factor2 <- games$mechanic.cluster.kmed
boxplot(outcome ~  factor1, col = unique(factor1 + 1 ), xlab = 'clusters on categories', ylab = 'average complexity' ) 
boxplot(outcome ~  factor2, col = unique(factor2 + 1 ), xlab = 'clusters on mechanics', ylab = 'average complexity' )

B = 1000
seed = 2022

perm_anova_twoway_interaction(outcome, factor1 = factor1, 
                              factor2 = factor2, iter = B)
# interaction term is significant 
perm_anova_twoway_factor12(outcome, factor1 = factor1, 
                           factor2 = factor2, interac = T, iter = B)
# first group is significant, second is not 
# we can try to focus on the model with only the first factor, making an anova multiple 
# ways in which the i-th group are "belonging or not to the ith group"

#### permutational anova multiple ways  complexity ~  category ####
outcome <- games$averageweight
dummy_vars <- fastDummies::dummy_cols(games$category.cluster.kmed)
boxplot(outcome ~  games$category.cluster.kmed, col = unique(games$category.cluster.kmed + 1)) 

# first col of dummy_vars is general data, is not a cluster, so i-th col is i-th -1 cluster
short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5]
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 5) # pval 0 5th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6]  
pval <- perm_anova_nway(outcome, short_formula, long_formula, 4) # pval 0 4th cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,5] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6]  
pval <- perm_anova_nway(outcome, short_formula, long_formula, 3) # pval 0 3rd cluster is significant 

short_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] 
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,3] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 2) # pval 1 2nd cluster is not significant 

short_formula <- outcome ~  dummy_vars[,4]  + dummy_vars[,5] + dummy_vars[,6]  
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,4]  + dummy_vars[,5] + dummy_vars[,6] 
pval <- perm_anova_nway(outcome, short_formula, long_formula, 1) # pval 0.0 1st cluster is significant 

## last group  
short_formula <- outcome ~  dummy_vars[,2] +dummy_vars[,4]  + dummy_vars[,5] + dummy_vars[,6]  
long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,4]  + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7]
pval <- perm_anova_nway(outcome, short_formula, long_formula, 5) # pval 0  significant

long_formula <- outcome ~ dummy_vars[,2] + dummy_vars[,4] + dummy_vars[,5] + dummy_vars[,6] + dummy_vars[,7] 

final_model <- aov(long_formula)
final_model$coefficient

## belonging to 3rd and 4th and 5th group increases the average, 6th and  1st decreases it 

special_cat1 <- ifelse(games$Wargame == 1 | games$Ancient == 1 | games$World.War.II == 1, 1, 0) 
special_cat2 <- ifelse( games$Economic == 1 | games$Card.Game == 1 | games$City.Building == 1, 1, 0)
special_cat3 <- ifelse( games$Fantasy == 1 | games$Fighting == 1 | games$Exploration == 1, 1, 0)

boxplot(games$averageweight ~ special_cat1, col = unique(special_cat1 + 2), pch = 19, cex = 0.5, ylab= "average rating", xlab = " War game, world war II, ancient")
boxplot(games$averageweight ~ special_cat2, col = unique(special_cat2 + 2), pch = 19, cex = 0.5, ylab= "average rating", xlab = " Economic")
boxplot(games$averageweight ~ special_cat3, col = unique(special_cat3 + 2), pch = 19, cex = 0.5, ylab= "average rating", xlab = " Fantasy")

perm_anova_oneway(games$averageweight, special_cat1) 
perm_anova_oneway(games$averageweight, special_cat2)
perm_anova_oneway(games$averageweight, special_cat3) 



#### gam for average weight  ####
games <- games[which(games$playingtime < 15000) ,]
plot(games[,c('averageweight','minage','playingtime','suggested_num_players')])
ggpairs(games[,c('averageweight','minage','playingtime','suggested_num_players')])
n <- round(dim(games)[1]*0.80) # for training 
set.seed(2022)
ind <- sample(1:dim(games)[1], size = n, replace = F)
y <- games$averageweight[ind]
x1 <- games$suggested_num_players[ind]
x2 <- games$minage[ind]
x3 <- games$playingtime[ind]

gam_ssplines = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr'))  
summary(gam_ssplines) # 70% R^2 adjusted with these three covariates

## shapley value 
df_train <- data.frame(x1 = x1, x2 = x2, x3 = x3)
shap_result_avgw_gam = shap.score.rank(xgb_model = gam_ssplines, 
                                   X_train = df_train,
                                   shap_approx = F, gam = T
)
var_importance(shap_result_avgw_gam, top_n=3)
names(shap_result_avgw_gam$mean_shap_score) <- c('x3','x2','x1')
names(shap_result_avgw_gam$shap_score) <- c('x1','x2','x3')
shap_long_avg = shap.prep(shap = shap_result_avgw_gam,
                          X_train = df_train, 
                          top_n = 3
)
plot.shap.summary(data_long = shap_long_avg)


#### XGboost for average weight
source("shap.R")
n <- round(dim(games)[1]*0.80)
set.seed(2022)
ind <- sample(1:dim(games)[1], size = n, replace = F)
y <- games$averageweight[ind]
x1 <- games$suggested_num_players[ind]
x2 <- games$minage[ind]
x3 <- games$playingtime[ind]

df_train <- as.matrix(data.frame(x1 = x1, x2 = x2, x3 = x3))
df_test <- as.matrix(data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind]))
xgb_train = xgb.DMatrix(data = df_train, label = y)
xgb_test = xgb.DMatrix(data = df_test, label = games$averageweight[-ind])

watchlist_xgb = list(train=xgb_train, test = xgb_test )
model = xgb.train(data = xgb_train, max.depth = 6, watchlist = watchlist_xgb, nrounds = 70)
# let's stay with 10 rounds 
xgb_avgw <- xgboost(data = xgb_train, 
                    nround = 10, 
                    objective="reg:linear",
                    label = y)
shap_result_avgw = shap.score.rank(xgb_model = xgb_avgw, 
                                   X_train = df_train,
                                   shap_approx = F
)
var_importance(shap_result_avgw, top_n=3)
shap_long_avgw = shap.prep(shap = shap_result_avgw,
                           X_train = df_train, 
                           top_n = 3
)
plot.shap.summary(data_long = shap_long_avgw)

### compare the two models 

df_test <- data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind])
n_test <- dim(games)[1] - n
y_test <- games$averageweight[-ind]
fit_test_xg <- predict(xgb_avgw, xgb_test)
sqrt(1/n_test * sum((fit_test_xg - y_test )^2))
fit_test_gam <- predict(gam_ssplines, df_test)
sqrt(1/n_test * sum((fit_test_gam - y_test )^2))


## trying to add clusters on categories for gam 
x4 <- as.factor(games$category.cluster.kmed[ind])
gam_ssplines_cl = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + x4) 
summary(gam_ssplines_cl) # 70% R^2 adj  -> better stay with simpler model 

### Gam lasso 

library(plsmselect)
library(dbarts)
games[,'category.cluster.kmed'] <- as.factor(games[,'category.cluster.kmed'])
games_lasso <- data.frame( averageweight = games$averageweight[ind], 
                           suggested_num_players = games$suggested_num_players[ind], 
                           minage = games$minage[ind], 
                           playingtime = games$playingtime[ind], 
                           X = games[ind,'category.cluster.kmed'])

games_lasso$X = makeModelMatrixFromDataFrame(data.frame(games_lasso$X))
gfit = gamlasso(averageweight ~ X +
                  s(suggested_num_players, bs="cs") +
                  s(minage, bs="cs") +
                  s(playingtime, bs="cs"),
                data=games_lasso,
                linear.penalty = "l1",
                smooth.penalty = "l1",
                seed=1)
summary(gfit)
X <- makeModelMatrixFromDataFrame(data.frame(games[-ind,'category.cluster.kmed']))
df_test <- data.frame( X = X,
                       suggested_num_players = games$suggested_num_players[-ind],
                       minage = games$minage[-ind], 
                       playingtime = games$playingtime[-ind], 
                       averageweight = games$averageweight[-ind])
y.test <- predict(gfit, newdata = df_test)
n_test <- dim(games)[1] - length(ind)
print(sqrt(1/n_test * sum((y.test - games$averageweight[-ind])^2)))

#### linear model for average ####

# a simple one: average ~ average_weight 
n <- round(dim(games)[1]*0.80) # for training 
set.seed(2022)
ind <- sample(1:dim(games)[1], size = n, replace = F)
y <- games$average[ind]
x <- games$averageweight[ind]
local_model = npreg(y ~ x,
                    ckertype = 'epanechnikov',
                    bws = 0.5) # set the bandwidth


# plot

x.grid = data.frame(x=seq(range(x)[1],range(x)[2],by=0.1))
preds=predict(local_model, newdata=x.grid, se=T)
se.bands=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
plot(x, y, xlim = range(x.grid$x), cex=.5, col="black", xlab = "average complexity", ylab = "average rating")
lines(x.grid$x, preds$fit, lwd =2, col="blue")
matlines(x.grid$x, se.bands, lwd =1, col="blue", lty =3)



# a more complex one 
y <- games$average[ind]
x1 <- games$suggested_num_players[ind]
x2 <- games$minage[ind]
x3 <- games$playingtime[ind]
x4 <- games$year[ind]
gam_ssplines_avg = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + s(x4, bs='cr'))  
summary(gam_ssplines_avg) # 46% 

df_train <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)
shap_result_avg_gam = shap.score.rank(xgb_model = gam_ssplines_avg, 
                                       X_train = df_train,
                                       shap_approx = F, gam = T
)
var_importance(shap_result_avg_gam, top_n=4)
names(shap_result_avg_gam$mean_shap_score) <- c('x4','x3','x1','x2')
names(shap_result_avg_gam$shap_score) <- c('x1','x2','x3','x4')
shap_long_avg = shap.prep(shap = shap_result_avg_gam,
                          X_train = df_train, 
                          top_n = 4
)
plot.shap.summary(data_long = shap_long_avg)

### XGboost average 

source("shap.R")


df_train <- as.matrix(data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4))
df_test <- as.matrix(data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind],
                                 x4 = games$year[-ind]))
xgb_train = xgb.DMatrix(data = df_train, label = y)
xgb_test = xgb.DMatrix(data = df_test, label = games$average[-ind])

watchlist_xgb = list(train=xgb_train, test = xgb_test )
model = xgb.train(data = xgb_train, max.depth = 6, watchlist = watchlist_xgb, nrounds = 70)

xgb_avg <- xgboost(data = xgb_train, 
                   nround = 40, 
                   objective="reg:linear",
                   label = y)
shap_result_avg = shap.score.rank(xgb_model = xgb_avg, 
                                  X_train = df_train,
                                  shap_approx = F
)
var_importance(shap_result_avg, top_n=4)
shap_long_avg = shap.prep(shap = shap_result_avg,
                          X_train = df_train, 
                          top_n = 4
)
plot.shap.summary(data_long = shap_long_avg)

### compare the models 
df_test <- data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind],
                                 x4 = games$year[-ind])

n_test <- dim(games)[1] - n
y_test <- games$average[-ind]
fit_test_xg <- predict(xgb_avg, xgb_test)
sqrt(1/n_test * sum((fit_test_xg - y_test )^2))
fit_test_gam <- predict(gam_ssplines_avg, df_test)
sqrt(1/n_test * sum((fit_test_gam - y_test )^2))
# gam works better between these two 
df_test_nploc <- data.frame(x = games$averageweight[-ind] )
fit_test_nploc <- predict(local_model, newdata = df_test_nploc )
rmse <- sqrt(1/n_test * sum((fit_test_nploc - y_test )^2))
rmse
 

## trying to add cluster on categories for gam
x6 <- as.factor(games$category.cluster.kmed[ind])
gam_avg_cl <- gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + s(x4, bs='cr') + x6)  
summary(gam_avg_cl) # 53.2 % -> better stay with the simpler model 



#### conformal prediction for gam models ####

## model on average weight
# x1 = suggested num players, x2 = minage, x3 = playingtime
train_gam=function(x,y,out=NULL){
  colnames(x)=c('var1','var2', 'var3')
  train_data=data.frame(y,x)
  gam_ssplines=gam(y ~ s(var1,bs='cr') + s(var2,bs='cr') + s(var3,bs='cr'), data=train_data)
}
predict_gam=function(obj, new_x){
  new_x=data.frame(new_x)
  colnames(new_x)=c('var1','var2', 'var3')
  predict.gam(obj,new_x)
}
new_game <- c(4, 6,180 )
alpha <- 0.1
conf_preds_split=conformal.pred.split(cbind(games$suggested_num_players,games$minage,games$playingtime),games$averageweight,new_game, alpha=alpha,verbose=T,train.fun = train_gam ,predict.fun = predict_gam, split = ind)
conf_preds_split$pred



#### some plots ####
library(mgcViz)
try <- getViz(gam_ssplines_avg)
pl1 <- plot(try, select = 1) + l_points(pch = 19, cex = 0.5) + l_fitLine(linetype = 1, col = 'blue')  +
  l_ciLine(colour = 4) + theme_get() + labs(y = c("f(suggested number of players)"), x = c("suggested number of players"))
print(pl1)
pl2 <- plot(try, select = 2) + l_points(pch = 19, cex = 0.5) + l_fitLine(linetype = 1, col = 'blue')  +
  l_ciLine(colour = 4) + theme_get() + labs(y = c("f(minimum age)"), x = c("minimum age"))
print(pl2)
pl3 <- plot(try, select = 3) + l_points(pch = 19, cex = 0.5) + l_fitLine(linetype = 1, col = 'blue')  +
  l_ciLine(colour = 4) + theme_get() + labs(y = c("f(playing time)"), x = c("playing time"))
print(pl3)
pl4 <- plot(try, select = 4) + l_points(pch = 19, cex = 0.5) + l_fitLine(linetype = 1, col = 'blue')  +
  l_ciLine(colour = 4) + theme_get() + labs(y = c("f(year)"), x = c("year"))
print(pl4)

#plot(try, select = 1) + l_dens(type = "cond") + l_fitLine() + l_ciLine()


try <- getViz(gam_ssplines)
pl1 <- plot(try, select = 1) + l_points(pch = 19, cex = 0.5) + l_fitLine(linetype = 1, col = 'blue')  +
  l_ciLine(colour = 4) + theme_get() + labs(y = c("f(suggested number of players)"), x = c("suggested number of players"))
print(pl1)
pl2 <- plot(try, select = 2) + l_points(pch = 19, cex = 0.5) + l_fitLine(linetype = 1, col = 'blue')  +
  l_ciLine(colour = 4) + theme_get() + labs(y = c("f(minimum age)"), x = c("minimum age"))
print(pl2)
pl3 <- plot(try, select = 3) + l_points(pch = 19, cex = 0.5) + l_fitLine(linetype = 1, col = 'blue')  +
  l_ciLine(colour = 4) + theme_get() + labs(y = c("f(playing time)"), x = c("playing time"))
print(pl3)



