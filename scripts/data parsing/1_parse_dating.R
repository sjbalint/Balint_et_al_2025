rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)

# import data -------------------------------------------------------------

dating.df <- read.csv("raw/raw_dating.csv") %>%
  mutate(location=factor(location,levels=c("North","Middle","South")))

# calculate accretion rate ------------------------------------------------

location.list <- dating.df %>%
  pull(location) %>%
  unique()

for (location in location.list){
  for (row in 2:nrow(dating.df)){
    if (dating.df[row-1,"location"]==location & dating.df[row,"location"]==location){
      dating.df[row,"n.years"] <- dating.df[row-1,"year.mean"]-dating.df[row,"year.mean"]
      dating.df[row,"accretion.rate.cmyr"] <- dating.df[row,"thickness.cm"]/dating.df[row,"n.years"] 
    }
  } 
}

saveRDS(dating.df,file="Rdata/dating.rds")
