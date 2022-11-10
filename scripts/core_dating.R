
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)


# import data -------------------------------------------------------------

dates.df <- read.csv("data/core_dating_2.csv",encoding="UTF-8")

load("Rdata/iso.Rdata")

mydata.df <- iso.df %>%
  select(location,depth.cm)


# perform regression ------------------------------------------------------

location.list <- dates.df %>%
  pull(location) %>%
  unique()

results.df <- data.frame()
row <- 0

for (mylocation in location.list){
  sub.df <- dates.df %>%
    filter(location==mylocation)
  row <- row+1
  my.lm <- lm(date.bottom~poly(depth.cm,2,raw=TRUE),sub.df)
  #my.lm <- lm(date.bottom~poly(depth.cm,2,raw=TRUE),sub.df)
  results.df[row,"location"] <- mylocation
  results.df[row,"x1"] <- my.lm$coefficients[2]
  results.df[row,"x2"] <- my.lm$coefficients[3]
  results.df[row,"b"] <- my.lm$coefficients[1]
}

mydata.df <- left_join(mydata.df,results.df)

mydata.df <- left_join(mydata.df,dates.df)


# extrapolate dates -------------------------------------------------------


for (row in 1:nrow(mydata.df)){
  if (is.na(mydata.df[row,"date.bottom"])==TRUE){
    mydata.df[row,"date.bottom"] <- 
      (mydata.df[row,"x2"]*(mydata.df[row,"depth.cm"]^2))+
      (mydata.df[row,"x1"]*mydata.df[row,"depth.cm"])+
      mydata.df[row,"b"]
    mydata.df[row,"extrapolated"] <- TRUE
  }
}

dates.df <- mydata.df %>%
  select(location,depth.cm,date.bottom)

save(dates.df,file="Rdata/dates.Rdata")