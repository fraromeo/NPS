games <- read.csv('clusterized_games1.csv')
games <- games[games$year > 2000, ]

#### functions ####

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













#### permutational anova  wanting + owned ~   cluster ####

boxplot(games$wanting + games$owned ~  games$cluster) 


B = 1000
seed = 2022
outcome <- games$wanting + games$owned 
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
p_val # no difference between groups 


















#### permutational anova  wanting + owned ~   low_avgw < avg_weight < high_avgw & cluster ####
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


















#### linear model for average weight  ####

plot(games[,c('averageweight','minage','playingtime','suggested_num_players')])
ggpairs(games[,c('averageweight','minage','playingtime','suggested_num_players')])

y <- games$averageweight
x1 <- games$suggested_num_players
x3 <- games$playingtime
x2 <- games$minage
gam_ssplines = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr'))  
summary(gam_ssplines) # 62% R^2 adjusted with these three covariates

## let's try also with cluster 
boxplot(games$averageweight ~ games$cluster)

x4 <- games$cluster
gam_ssplines_cl <- gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + x4)
summary(gam_ssplines_cl)

x4 <- ifelse( games$cluster == 1,1, 0)
x5 <- ifelse( games$cluster == 2,1, 0)
gam_ssplines_cl <- gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + x4 + x5)
summary(gam_ssplines_cl)
## not so great 

## let's try with year 

x4 <- games$year
gam_ssplines_yr<- gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + s(x4,bs = 'cr'))
summary(gam_ssplines_yr) ## R^2 adj is just 62.5% 
hist(gam_ssplines_yr$residuals)
qqnorm(gam_ssplines_yr$residuals)

anova(gam_ssplines_yr,gam_ssplines, test = "F") # the models are different
# but the R2 adj doesn't increase 

#### permutational anova  averageweight ~   cluster ####
outcome <- games$averageweight
boxplot(outcome~  games$cluster) 


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



















#### linear model for average ####

# a simple one: average ~ average_weight 
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

# a more complex one 

y <- games$average

x1 <- games$minage
x2 <- games$suggested_num_players
x3 <- games$playingtime
x4 <- games$averageweight
x5 <- games$year
gam_ssplines_avg = gam(y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + s(x4, bs='cr') + s(x5, bs='cr'))  
summary(gam_ssplines_avg) # 46% 

dummy_vars <- fastDummies::dummy_cols(games$cluster)





