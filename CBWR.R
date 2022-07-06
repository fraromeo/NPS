##### Beginning #####
rm(list=ls())
#install.packages("remotes")
#remotes::install_github("beansrowning/cbRw")
library(cbRw)
library(dplyr)
library(tidyverse)
library(tidytext)
library(caret)
library(cowplot)

##### Loading #####
games <-read.csv('games_cleaned.csv')
##### cleaning mechanic ######
games <- games %>%
  filter(!is.na(category))

games.sep1 <- games %>%
  separate(mechanic, c('cat1', 'cat2', 'cat3'), sep = ", ") %>%
  mutate(cat12 = if_else(is.na(cat1), 'Not Av', cat1)) %>%
  mutate(cat12 = if_else(cat12=="", 'Not Av', cat12)) %>%
  dplyr::select(cat12)

games.sep2 <- games %>%
  separate(mechanic, c('cat1', 'cat2', 'cat3'), sep = ", ") %>%
  mutate(cat22 = if_else(is.na(cat2), 'Not Av', cat2)) %>%
  mutate(cat22 = if_else(cat22=="", 'Not Av', cat22)) %>%
  dplyr::select(cat22)

games.sep3 <- games %>%
  separate(mechanic, c('cat1', 'cat2', 'cat3'), sep = ", ") %>%
  mutate(cat32 = if_else(is.na(cat3), 'Not Av', cat3)) %>%
  mutate(cat32 = if_else(cat32=="", 'Not Av', cat32)) %>%
  dplyr::select(cat32)

remove_first5 <- function(name) {
  new_name = substring(name, 6)
  return (new_name)
}

ChangeColnames <- function(x) {
  colnames(x) <- remove_first5(colnames(x))
  x
}

catmat1 <- model.matrix(~cat12-1, games.sep1)
catmat1 <- ChangeColnames(catmat1)

catmat2 <- model.matrix(~cat22-1, games.sep2)
catmat2 <- ChangeColnames(catmat2)

catmat3 <- model.matrix(~cat32-1, games.sep3)
catmat3 <- ChangeColnames(catmat3)

df.cat1 <- data.frame(catmat1)
df.cat1$id <- games$id

df.cat2 <- data.frame(catmat2)
df.cat2$id <- games$id


df.cat3 <- data.frame(catmat3)
df.cat3$id <- games$id

dm <-bind_rows(df.cat1, 
               df.cat2,
               df.cat3
) %>%
  # evaluate following calls for each value in the rowname column
  group_by(id) %>% 
  # add all non-grouping variables
  summarise_all(sum)

dm$V1 <- NULL
games <- merge(games,dm, by='id')
games = games %>% 
  rename(
    Prisoners.Dilemma = X.Prisoners.Dilemma.
  )

games = games[, colSums(is.na(games)) != nrow(games)]
games$Not.Av <- NULL

tab = colSums(games[,c(103:237)])
summary(tab)
few_sample = names(tab)[tab < 1000]
games[,few_sample] <- NULL

##### Cleaning useless columns #####

cols.dont.want <- c("X", "Unnamed..0", 
                    "description", "suggested_playerage", 
                    "suggested_language_dependence", "category",
                    "family", "designer", "artist", "publisher") # if you want to remove multiple columns

games <- games[, ! names(games) %in% cols.dont.want, drop = F]
names(games)

##### Find Outliers ####

# technique to find outliers robustly in theory

cat.indices <- 93:105
for (ind in cat.indices){
  games[,ind] <- factor(games[, ind])
}

value <- cbrw(games[, cat.indices])
plot(games$averageweight, games$average, col=ifelse(value$score > quantile(value$score, 0.8), 'red', 'black'))
boxplot(value$score)

out.ind <- value$score > quantile(value$score, 0.995)
out.id <- games$id[out.ind]
out.name <- games$name[out.ind]
summary(games$numratings[out.ind])
summary(games$numratings[-out.ind])

for (id in out.id){
  print(games[games$id == id,'name'])
  print(games[games$id == id, games[games$id == id,] == 1])
}


##### RockCluster #####
# robust clustering technique
library(cba)
for (ind in cat.indices){
  games[,ind] <- as.factor(games[, ind])
}
x <- as.dummy(games[,cat.indices])
#cl <- rockCluster(x, n=10)
