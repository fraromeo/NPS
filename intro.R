rm (list = ls())

# Libraries
library(tidyverse)
library(tidytext)
library(caret)
library(cowplot)

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
  rename(
    year = yearpublished,
    category = boardgamecategory,
    mechanic = boardgamemechanic,
    family = boardgamefamily,
    designer = boardgamedesigner,
    artist = boardgameartist,
    publisher = boardgamepublisher,
    numratings = usersrated,
    name = primary
  )

games <- games %>%
  filter(!is.na(category))

names(games)

#Games with published year = 0 -> fake
zero.year = games$year == 0
games[zero.year ,c('name')] # they are clearly more recent -> not trustworthy
games = games[!zero.year,]

#### Exploration #### 
indexes.shuffle <- sample(dim(games)[1])
plot(games$average[indexes.shuffle])

plot(games$bayesaverage, games$average)
plot(games$averageweight, games$average)
plot(games$year[games$year > 1990], games$average[games$year > 1990])

indexes.shapiro = sample(dim(games)[1], 5000)
shapiro.test(games$average[indexes.shapiro]) #NOT GAUSSIAN -> YES

#### Filter games with single category -> just to try ####
games$cnt<-unlist(lapply(str_split(games$category, ","), length))
single_cat<-games %>% 
  filter(games$cnt==1)
glimpse(unique(single_cat$category))

top_cat<-single_cat %>% 
  filter(!is.na(category)) %>% 
  group_by(category) %>% 
  tally() %>% 
  top_n(74, n) %>% 
  arrange(desc(n))

top_cat
games_df<-games %>% filter(category %in% top_cat$category)

#### One Hot Encoding ####



library(hrbrthemes)
library(viridis)

# Boxplot + jitter
games_df %>%
  ggplot( aes(x=category, y=average, fill=category)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

# Violin basic
games_df %>%
  ggplot( aes(x=category, y=average, fill=category)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

# Scatterplot

pairs(games[games$playingtime < 2000, c('minplayers', 'maxplayers', 'playingtime', 'average')])
library(GGally)
ggpairs(games[,c('numratings', 'averageweight', 'playingtime', 'average')]) 


