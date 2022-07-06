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

##### Computing mechanic MCA ####
#mech.indices <- 33:170
mech.indices <- 93:105#103:170
library(FactoMineR)

for (ind in mech.indices){
  games[,ind] <- factor(games[, ind])
}

mca.indices <- c(mech.indices, 13)
res.mca = MCA(games[,mca.indices], quanti.sup=14)

barplot(res.mca$eig[,1], main = "Eigenvalues", 
        names.arg = paste("Dim", 1:nrow(res.mca$eig), sep = ""))
abline(h = 1 / (length(mca.indices)-1), col='red')

plot(cumsum(res.mca$eig[,1]), type='b', axes=T, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')

options(ggrepel.max.overlaps = Inf)
plot(res.mca,invisible=c("ind"), axes = c(1, 3), selectMod="contrib 10", unselect=0.9)
plot(res.mca,invisible=c("ind"), axes = c(1, 2), selectMod="cos2 10", unselect=1)


res.hcpc = HCPC(res.mca)


##### Fetaure Clustering #####
library("corrplot")
mech.indices <- 93:105#92:136
for (ind in mech.indices){
  games[,ind] <- as.numeric(games[, ind])
}
cor.mat <- round(cor(games[,mech.indices]),2)

x11()
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

#----- Dissimilarity Matrix -----#
library(cluster) 
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
for (ind in mech.indices){
  games[,ind] <- as.factor(games[, ind])
}
gower.dist <- daisy(games[ ,mech.indices], metric = c("gower"))
