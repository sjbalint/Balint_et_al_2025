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
  
  grainsize.df <- data.df %>%
    mutate(grainsize=as.numeric(row.names(data.df)),
           class = cut(grainsize, breaks = c(0, 4.04, 62.5, 2000,3500), 
                       labels = c("clay", "silt", "sand", "gravel"), include.lowest = TRUE)) %>%
    select(-grainsize) %>%
    group_by(class) %>%
    summarize_all(sum) %>%
    select(-class) %>%
    t() %>%
    as.data.frame() %>%
    mutate_all(signif, digits=3)
  
  
  colnames(grainsize.df) <- c("clay", "silt", "sand", "gravel")
  
  log.df <- bind_cols(log.df, grainsize.df)
  
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

# calculate detailed size classes -----------------------------------------

data.df <- widen_cores(all_cores.df,"North")

size.list <- c(2,4,8,16,31,62,125,250,500,1000,2000,4000)

class.list <- c("Clay","V.F. Silt","F. Silt","M. Silt","C. Silt",
                "V.C. Silt","V.F. Sand","F. Sand",
                "Sand","C. Sand","V.C. Sand","V.F. Gravel")

size_classes <- function(data.df, size.list, class.list){
  
  grainclass.df <- data.df %>%
    mutate(grainsize=as.numeric(row.names(data.df)),
           class = cut(grainsize, breaks = c(0, size.list), 
                       labels = class.list, include.lowest = TRUE)) %>%
    select(-grainsize) %>%
    group_by(class) %>%
    summarize_all(sum) %>%
    pivot_longer(-class, names_to="depth.cm", values_to="percentage") %>%
    mutate(depth.cm=as.numeric(depth.cm))
  
  depth.df <- grainclass.df %>%
    select(depth.cm) %>%
    unique() %>%
    mutate(thickness.cm=2)
  
  for (row in 1:(nrow(depth.df)-1)){
    depth.df[row,"thickness.cm"] = depth.df[row+1,"depth.cm"]-depth.df[row,"depth.cm"]
  }
  
  grainclass.df <- left_join(grainclass.df,depth.df)

  return(grainclass.df)
  
}

# perform classification ------------------------------------------------------

north.df <- widen_cores(all_cores.df,"North") %>%
  size_classes(size.list, class.list) %>%
  mutate(location="North")

middle.df <- widen_cores(all_cores.df,"Middle")%>%
  size_classes(size.list, class.list) %>%
  mutate(location="Middle")

south.df <- widen_cores(all_cores.df,"South")%>%
  size_classes(size.list, class.list)%>%
  mutate(location="South")

class.df <- bind_rows(north.df,middle.df,south.df) %>%
  mutate(location=factor(location, levels=c("North","Middle","South")))

# export results ----------------------------------------------------------

saveRDS(grain.df,file="Rdata/grainstats.rds")

saveRDS(class.df,file="Rdata/grainclass.rds")



