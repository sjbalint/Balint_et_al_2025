rm(list = ls()) #clear environment

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)
library(G2Sd)
library(cowplot)
library(ggforce)


# example -----------------------------------------------------------------

#data.df <- granulo

grain_stats <- function(data.df){
  
  manual.df <- data.df %>%
    mutate(phi = log2(as.numeric(rownames(data.df)))) %>%
    pivot_longer(-phi) %>%
    mutate(mean.phi=phi*value) %>%
    group_by(name) %>%
    mutate(mean.phi = sum(mean.phi)/100) %>%
    ungroup() %>%
    mutate(sd.phi=value*(phi-mean.phi)^2) %>%
    group_by(name) %>%
    mutate(sd.phi=sqrt(sum(sd.phi)/100)) %>%
    ungroup() %>%
    select(-c("value","phi")) %>%
    unique()
  
  summary.df <- granstat(data.df)
  
  myrows <- c("Kurtosis.fw.um","Skewness.fw.um") #sorting = standard deviation
  
  log.df <- summary.df[rownames(summary.df) %in% myrows, ] %>%
    t() %>%
    data.frame() %>%
    mutate_all(as.numeric)
  
  colnames(log.df) <- c("skewness.um","kurtosis.um")
  
  log.df$mean.phi <- manual.df$mean.phi
  log.df$sd.phi <- manual.df$sd.phi
  
  log.df$name <- rownames(log.df)
  
  return(log.df)
}


# do it for real ----------------------------------------------------------

load("Rdata/dating.Rdata")

all_cores.df <- read_excel("raw/grainsize/grainsize_tidy.xlsx") %>%
  select(-c(Replicate,Pseudoreplicate,Core)) %>%
  group_by(Location,Depth) %>%
  summarize_all(mean, na.rm=TRUE) %>%
  ungroup()

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
  
  return(temp.df)
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

grain.df <- left_join(grain.df,dating.df) %>%
  drop_na(century)

grainstats.df <- grain.df

save(grainstats.df,file="Rdata/grainstats.Rdata")