rm(list = ls()) #clear environment

library(tidyverse)
library(readxl)
library(G2Sd)

# import data -------------------------------------------------------------

all_cores.df <- read_excel("raw/grainsize.xlsx") %>%
  select(-c(Replicate,Pseudoreplicate,Core)) %>%
  group_by(Location,Depth) %>%
  summarize_all(mean, na.rm=TRUE) %>%
  ungroup()

# function to format data for G2Sd ----------------------------------------

widen_cores <- function(data.df,location){
  temp.df <- data.df %>%
    filter(Location==location) %>%
    select(-Location)
  
  depths <- temp.df %>%
    pull(Depth)
  
  temp.df <- temp.df %>%
    select(-Depth) %>%
    t() %>%
    data.frame()
  
  colnames(temp.df) <- depths
  
  rownames(temp.df) <- as.numeric(rownames(temp.df))
  
  temp.df <- temp.df[nrow(temp.df):1,]
  
  return(temp.df)
}

# function to calculate statistics ----------------------------------------

grain_stats <- function(data.df){
  
  summary.df <- granstat(data.df)
  
  myrows <- c("Mean.fw.phi","Sd.fw.phi","Kurtosis.fw.phi","Skewness.fw.phi") #sorting = standard deviation
  
  log.df <- summary.df[rownames(summary.df) %in% myrows, ] %>%
    t() %>%
    data.frame() %>%
    mutate_all(as.numeric)
  
  colnames(log.df) <- c("mean.phi","sd.phi","skewness.phi","kurtosis.phi")
  
  log.df$name <- rownames(log.df)
  
  return(log.df)
}

# perform statistics ------------------------------------------------------

north.df <- widen_cores(all_cores.df,"North") %>%
  grain_stats() %>%
  mutate(location="North")

middle.df <- widen_cores(all_cores.df,"Middle")%>%
  grain_stats() %>%
  mutate(location="Middle")

south.df <- widen_cores(all_cores.df,"South")%>%
  grain_stats()%>%
  mutate(location="South")

grain.df <- bind_rows(north.df,middle.df,south.df) %>%
  mutate(depth.cm=as.numeric(substr(name,2,20))) %>%
  select(-name)

# export results ----------------------------------------------------------

saveRDS(grain.df,file="Rdata/grainstats.rds")
