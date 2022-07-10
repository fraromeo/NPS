rm(list=ls())
games <- read.csv('clusterized_games3.csv')


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
  print(names(sort(coocc))[1:num_cat])
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





#### characterization of groups ####

most_common(4,3)

#### permutational anova  wanting + owned ~ mechanic + category ####
outcome <- games$wanting + games$owned 
factor1 <- games$category.cluster.kmed
factor2 <- games$mechanic.cluster.kmed
boxplot(outcome ~  factor1, col = unique(factor1 + 1 )) 
boxplot(outcome ~  factor2, col = unique(factor2 + 1 ))

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

## belonging to 4th and 5th group increases the average, 6th and 2nd and 1st decreases it 








#### permutational anova  complexity ~ mechanic + category ####
outcome <- games$averageweight
factor1 <- games$category.cluster.kmed
factor2 <- games$mechanic.cluster.kmed
boxplot(outcome ~  factor1, col = unique(factor1 + 1 ), xlab = 'categories') 
boxplot(outcome ~  factor2, col = unique(factor2 + 1 ), xlab = 'mechanics')

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








#### gam for average weight  ####

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

## trying to add clusters on categories for xgboost
df_train <- as.matrix(data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = games$category.cluster.kmed[ind]))
df_test <- as.matrix(data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind], x4 = games$category.cluster.kmed[-ind]))
xgb_train = xgb.DMatrix(data = df_train, label = y)
xgb_test = xgb.DMatrix(data = df_test, label = games$averageweight[-ind])
watchlist_xgb = list(train=xgb_train, test = xgb_test )
model = xgb.train(data = xgb_train, max.depth = 6, watchlist = watchlist_xgb, nrounds = 70)
xgb_avgw_cl <- xgboost(data = xgb_train, 
                    nround = 10, 
                    objective="reg:linear",
                    label = y)
shap_result_avgw_cl = shap.score.rank(xgb_model = xgb_avgw_cl, 
                                   X_train = df_train,
                                   shap_approx = F
)
var_importance(shap_result_avgw_cl, top_n=4)
shap_long_avgw_cl = shap.prep(shap = shap_result_avgw_cl,
                           X_train = df_train, 
                           top_n = 4
)
plot.shap.summary(data_long = shap_long_avgw_cl)


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
plot(x, y, xlim = range(x.grid$x), cex=.5, col="darkgrey")
lines(x.grid$x, preds$fit, lwd =2, col="blue")
matlines(x.grid$x, se.bands, lwd =1, col="blue", lty =3)



# a more complex one 
y <- games$average[ind]
x1 <- games$suggested_num_players[ind]
x2 <- games$minage[ind]
x3 <- games$playingtime[ind]
x4 <- games$averageweight[ind]
x5 <- games$year[ind]
gam_ssplines_avg = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + s(x4, bs='cr') + s(x5, bs='cr'))  
summary(gam_ssplines_avg) # 53% 

df_train <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)
shap_result_avg_gam = shap.score.rank(xgb_model = gam_ssplines_avg, 
                                       X_train = df_train,
                                       shap_approx = F, gam = T
)
var_importance(shap_result_avg_gam, top_n=5)
names(shap_result_avg_gam$mean_shap_score) <- c('x4','x5','x2','x3','x1')
names(shap_result_avg_gam$shap_score) <- c('x1','x2','x3','x4','x5')
shap_long_avg = shap.prep(shap = shap_result_avg_gam,
                          X_train = df_train, 
                          top_n = 5
)
plot.shap.summary(data_long = shap_long_avg)

### XGboost average 

source("shap.R")


df_train <- as.matrix(data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5))
df_test <- as.matrix(data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind],
                                 x4 = games$averageweight[-ind], x5 = games$year[-ind]))
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
var_importance(shap_result_avg, top_n=5)
shap_long_avg = shap.prep(shap = shap_result_avg,
                          X_train = df_train, 
                          top_n = 5
)
plot.shap.summary(data_long = shap_long_avg)

### compare the models 
df_test <- data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind],
                                 x4 = games$averageweight[-ind], x5 = games$year[-ind])

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
# the local method is the best on the test set 

## trying to add cluster on categories for gam

x6 <- as.factor(games$category.cluster.kmed[ind])
gam_avg_cl <- gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + s(x4, bs='cr') + s(x5, bs='cr') + x6)  
summary(gam_avg_cl) # 53.2 % -> better stay with the simpler model 

## trying to add cluster on categories for xgboost

df_train <- as.matrix(data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = games$category.cluster.kmed[ind]))
df_test <- as.matrix(data.frame( x1 = games$suggested_num_players[-ind], x2 = games$minage[-ind], x3 = games$playingtime[-ind],
                                 x4 = games$averageweight[-ind], x5 = games$year[-ind], x6 = games$category.cluster.kmed[-ind]))

xgb_train = xgb.DMatrix(data = df_train, label = y)
xgb_test = xgb.DMatrix(data = df_test, label = games$average[-ind])
watchlist_xgb = list(train=xgb_train, test = xgb_test )
model = xgb.train(data = xgb_train, max.depth = 6, watchlist = watchlist_xgb, nrounds = 70)
# model is worse so also here is better the model without categories 


games$new_cat <- ifelse(games$category.cluster.kmed == 4 |games$category.cluster.kmed == 5 , 2 , 0)
zero_ind <- which(games$new_cat == 0)
games$new_cat[zero_ind] <- ifelse(games$category.cluster.kmed[zero_ind] == 1 |games$category.cluster.kmed[zero_ind] == 2 | games$category.cluster.kmed[zero_ind] == 6 , 1 , 0)
x6 <- as.factor(games$new_cat[ind])

gam_ssplines_avg_cl = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr')  + s(x5, bs='cr'))  
summary(gam_ssplines_avg_cl)

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
pl <- plot(try) + l_points(pch = 19, cex = 0.5) + l_fitLine(linetype = 1, col = 'blue')  +
  l_ciLine(colour = 4) + theme_get() + labs(title = NULL)
print(pl, pages = 1)
#plot(try, select = 1) + l_dens(type = "cond") + l_fitLine() + l_ciLine()
plotRGL(sm(try, 1), fix = c("x3" = 0), residuals = TRUE)


#### GAMlasso ####

games$mat <- model.matrix(~ games[,24:82] ,data = games)[,-1]
gfit <- gamlasso(wanting ~ mat + s(year, k = 5 , bs = 'ts')+ s(suggested_num_players, k = 5, bs = 'ts')+ s(playingtime, k = 5, bs = 'ts')+ s(minage, k = 5 ,bs = 'ts')+ s(averageweight, k = 5 ,bs = 'ts'), data = games)
summary(gfit)

gfit <- gam(wanting ~ Childrens.Game + s(year, bs = 'cs')+ s(suggested_num_players, bs = 'cs')+ s(playingtime, bs = 'cs')+ s(minage, bs = 'cs')+ s(averageweight, bs = 'cs'), data = games)
summary(gfit)



