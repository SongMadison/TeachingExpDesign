# using dplyr

rm(list=ls())  # clean up work space
setwd("/Users/SongWang/Stat/Teaching/stat424/code/project1")


Plastic <- read.csv("Project 1 data.csv")

Plastic <- Plastic[-9,]

require(dplyr)
mean.temp = Plastic %>% 
  dplyr::group_by(Reactor) %>%
  dplyr::summarise(mean(Temp))

mean.temp

cor(Plastic$Temp, Plastic$Conv)
corr.Reactor = Plastic %>%
    dplyr::group_by(Reactor) %>%
    dplyr::summarise(cor(Temp,Conv))
corr.Reactor
