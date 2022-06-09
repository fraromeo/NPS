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













