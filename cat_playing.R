##### Intro ####

rm(list=ls())

##### Libraries ####

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

##### Functions ####

compute.cooccurrence <- function(X) {
  X <- as.matrix(X)
  out <- crossprod(X)  # Same as: t(X) %*% X
  diag(out) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)
  out
}
##### Begin ####

games <- readRDS("data.RDS")

glimpse(games)
summary(games)

dim(games)
dim(games[games$year > 1990, ]) 

category.indices <- 31:100
compute.cooccurrence(games[,category.indices])

write.csv(games, 'games_da_ripulire.csv')
