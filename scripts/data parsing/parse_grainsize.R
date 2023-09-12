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

size_classes <- c("Clay","V.F. Silt","F. Silt","M. Silt","C. Silt","V.C. Silt",
                  "V.F. Sand","F. Sand","Sand","C. Sand","V.C. Sand","V.F. Gravel")

approx_classes <- c("Clay","Clay","Clay","Silt","Silt","Silt",
                         "Sand","Sand","Sand","Sand","Sand","Gravel")

grainsizes.df <- data.frame(size_classes,approx_classes,
                            c(2,4,8,16,31,62,125,250,500,1000,2000,4000))
colnames(grainsizes.df) <- c("Fine Class","Rough Class","Micrometers")

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

grain.df$`Fine Class` <- factor(grain.df$`Fine Class`,levels=size_classes)

if (date_shift==TRUE){
  for (row in 1:nrow(grain.df)){
    if (grain.df[row,"Location"]=="South"){
      grain.df[row,"Depth"] <- grain.df[row,"Depth"]+12
    }
    if (grain.df[row,"Location"]=="North"){
      grain.df[row,"Depth"] <- grain.df[row,"Depth"]+4
    }
  }
}

colnames(grain.df) <- c("location","depth.cm","grainsize.um","class.pct","class.fine","class.rough")

save(grain.df,file="Rdata/grainsize.Rdata")
