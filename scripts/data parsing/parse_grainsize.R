rm(list = ls()) #clear environment

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)
#library(G2Sd)

date_shift <- FALSE

grain.df <- read_excel("raw/grainsize/grainsize_tidy.xlsx") %>%
  select(-c(Replicate,Pseudoreplicate,Core)) %>%
  group_by(Location,Depth) %>%
  summarize_all(mean) %>%
  pivot_longer(!c(Location,Depth),names_to="Micrometers",values_to="Percentage") %>%
  ungroup()

grain.df$Location <- factor(grain.df$Location,levels=c("North","Middle","South"))

grain.df$Micrometers <- as.numeric(grain.df$Micrometers)

mysizes.df <- data.frame(as.numeric(unique(grain.df$Micrometers)))
colnames(mysizes.df) <- c("Micrometers")

grainsizes.df <- read_excel("raw/grainsize/grainsize_classes.xlsx")

for (row in 1:nrow(mysizes.df)){
  for (row2 in 1:nrow(grainsizes.df)){
    if (mysizes.df[row,"Micrometers"]<grainsizes.df[row2,"Micrometers"]){
      mysizes.df[row,"Fine Class"] <- grainsizes.df[row2,"Fine Class"]
      mysizes.df[row,"Rough Class"] <- grainsizes.df[row2,"Rough Class"]
      break
    }
  }
}

grain.df <- left_join(grain.df,mysizes.df)

colnames(grain.df) <- c("location","depth.cm","grainsize.um","class.pct","class.fine","class.rough")

save(grain.df,file="Rdata/grainsize.Rdata")
