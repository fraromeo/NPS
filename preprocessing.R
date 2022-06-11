rm (list = ls())
tilda = '~' # clown 

# Libraries
library(tidyverse)
library(tidytext)
library(caret)
library(cowplot)
library(dplyr)

details <-read.csv('games_detailed_info.csv') %>% 
          mutate(boardgamecategory = gsub("\\[", "", boardgamecategory), boardgamecategory = gsub("\\]", "", boardgamecategory), boardgamecategory = gsub(" \\/ ", "", boardgamecategory), boardgamecategory=gsub("\\/", "", boardgamecategory), boardgamecategory=gsub("'", "", boardgamecategory), 
                 boardgamemechanic = gsub("\\[", "", boardgamemechanic), boardgamemechanic = gsub("\\]", "", boardgamemechanic), boardgamemechanic = gsub(" \\/ ", "", boardgamemechanic), boardgamemechanic=gsub("\\/", "", boardgamemechanic), boardgamemechanic=gsub("'", "", boardgamemechanic),
                 boardgamefamily = gsub("\\[", "", boardgamefamily), boardgamefamily = gsub("\\]", "", boardgamefamily), boardgamefamily = gsub(" \\/ ", "", boardgamefamily), boardgamefamily=gsub("\\/", "", boardgamefamily), boardgamefamily=gsub("'", "", boardgamefamily),
                 boardgamepublisher = gsub("\\[", "", boardgamepublisher), boardgamepublisher = gsub("\\]", "", boardgamepublisher), boardgamepublisher = gsub(" \\/ ", "", boardgamepublisher), boardgamepublisher=gsub("\\/", "", boardgamepublisher), boardgamefamily=gsub("'", "", boardgamepublisher))
glimpse(details)
summary(details)


#### Cleaning Data ####
games <- details

names(games)
# Remove Sector Rank
games[c('Amiga.Rank', 'Atari.ST.Rank', 'Arcade.Rank', "Commodore.64.Rank", "Video.Game.Rank",
        "Accessory.Rank", "RPG.Item.Rank" , "Children.s.Game.Rank", "Customizable.Rank", "War.Game.Rank", 
        "Thematic.Rank", "Abstract.Game.Rank", "Party.Game.Rank", "Board.Game.Rank", "Strategy.Game.Rank", "Family.Game.Rank")] <- NULL 
# Remove useless features
games[c("X","thumbnail","image", "type", "alternate")] <- NULL
games[c("boardgameintegration", "boardgamecompilation", "boardgameimplementation", "boardgameexpansion")] <- NULL
# Always to 0
games[c("median")] <- NULL


names(games)

games = games %>% 
  dplyr::rename(year = yearpublished,
    category = boardgamecategory,
    mechanic = boardgamemechanic,
    family = boardgamefamily,
    designer = boardgamedesigner,
    artist = boardgameartist,
    publisher = boardgamepublisher,
    numratings = usersrated,
    name = primary
  )


names(games)

#Games with published year = 0 -> fake
zero.year = games$year <= 0
games[zero.year ,c('name')] # they are clearly more recent -> not trustworthy
games = games[!zero.year,]


#### One Hot Encoding ####

games <- games %>%
  filter(!is.na(category))

games.sep1 <- games %>%
  separate(category, c('cat1', 'cat2', 'cat3'), sep = ", ") %>%
  mutate(cat12 = if_else(is.na(cat1), 'Not Av', cat1)) %>%
  select(cat12)

games.sep2 <- games %>%
  separate(category, c('cat1', 'cat2', 'cat3'), sep = ", ") %>%
  mutate(cat22 = if_else(is.na(cat2), 'Not Av', cat2)) %>%
  select(cat22)

games.sep3 <- games %>%
  separate(category, c('cat1', 'cat2', 'cat3'), sep = ", ") %>%
  mutate(cat32 = if_else(is.na(cat3), 'Not Av', cat3)) %>%
  select(cat32)

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

# add rownames as a column in each data.frame and bind rows
dm <-bind_rows(df.cat1, 
          df.cat2,
          df.cat3
          ) %>%
  # evaluate following calls for each value in the rowname column
  group_by(id) %>% 
  # add all non-grouping variables
  summarise_all(sum)


##### JOIN GAMES AND CATEGORY DATASET ####

dm$V1 <- NULL
games <- merge(games,dm, by='id')
games = games %>% 
  rename(
    Childrens.Game = X.Childrens.Game.
  )

games = games[, colSums(is.na(games)) != nrow(games)]


tab = colSums(games[,c(31:106)])
few_sample = names(tab)[tab < 73]

games[,few_sample] <- NULL

#games[,c(31:100)] <- factor(games[,c(31:100)])

# further cleaning 
games <- games[!games[,'numweights'] == 0,] # zero averageweight 


saveRDS(games, file='data.RDS')


### create table with commong categories 
which(colnames(games) == 'Childrens.Game' | colnames(games) == 'Zombies')
first_cat <- 31
last_cat <- 100
two_way_table <- matrix(nrow = last_cat - first_cat, ncol =last_cat - first_cat )
dist_mat <- matrix(nrow = last_cat - first_cat, ncol = last_cat - first_cat)
for (i in first_cat:last_cat){
  for(j in first_cat:last_cat){
    cat_rows <- which(games[,i] == 1)
    
    common <- length(which(games[cat_rows,j] == 1))
    two_way_table[i-first_cat,j-first_cat] <- common
    if (common == 0){
      dist_mat[i-first_cat,j-first_cat] <- 2
    }else{
      dist_mat[i-first_cat,j-first_cat] <- 1/common
    }
  }
}

dist_mat <- as.dist(dist_mat)
rownames(dist_mat) <- colnames(games[first_cat, last_cat])
dendo <- hclust(dist_mat, method='complete')
plot(dendo, main='complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(dendo, k=25) # reduce categories from 59 to 25 


### ideas for cleaning the dataset: 

# games with not available minage (also suggested_player_age seems to have only 0 votes)
ind <- which(games$minage == 0) 
hist(games$average)
hist(games$average[-ind]) # very similar to the one of full dataset
hist(games$average[ind] ) # high averages
length(ind) # not so many if compared to nrows

# games with "significant" reviews (https://jvanelteren.github.io/blog/2022/01/19/boardgames.html)
ind <- which(games$numcomments/games$numratings < 0.2)
hist(games$average)
hist(games$average[-ind])
hist(games$average[ind] )
length(ind) #  not so many if compared to nrows

# games with less than 1000 reviews (https://jvanelteren.github.io/blog/2022/01/19/boardgames.html)

summary(games$numratings)
ind <- which(games$numratings < 400) 
length(ind)/dim(games)[1] # 74% of games discarded -> maybe too much 
hist(games$average)
hist(games$average[-ind])
hist(games$average[ind] )
length(ind) # quite a lot 





