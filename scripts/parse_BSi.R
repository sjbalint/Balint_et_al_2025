rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)

# import data -------------------------------------------------------------

weights.df <- read_excel("raw/silica/BSi_weights.xlsx") #weights

seal.df <- read.csv("raw/silica/silica_12092022.csv") %>% #data from SEAL
  select(c("Sample.ID","SiO2.uM")) %>%
  group_by(Sample.ID) %>%
  summarize_all(mean) %>%
  ungroup()


# parse sample ID ---------------------------------------------------------

seal.df$id <- str_sub(seal.df$Sample.ID,0,-2) %>% #return numeric sample identifier
  as.numeric()

seal.df$time <- str_sub(seal.df$Sample.ID,start=-1) #return letter time indentifier

#convert letter time indentifier to number of hours
seal.df$time.hr <- factor(seal.df$time,levels=c("A","B","C"), labels=c(3,4,5)) %>%
  as.numeric()

silica.df <- left_join(seal.df,weights.df)
  #drop_na(replicate)


# calculate percent SiO ---------------------------------------------------

silica.df$SiO2.mg <- silica.df$SiO2.uM * 60.08 * 0.04 * 10 * 0.1

silica.df$SiO2.prct <- silica.df$SiO2.mg/silica.df$final.mg

silica.df <- silica.df %>%
  select(c("id","time.hr","replicate","location","depth.cm","SiO2.prct"))


# linear regression -------------------------------------------------------

id.list <- silica.df %>% #create list of sample ids
  pull(id) %>%
  unique()

results.list <- list()

for (myid in id.list){
  temp.df <- silica.df %>%
    filter(id==myid)
  #recode time variable to always start with 0 so intercept is calculated back to first time point
  #mintime <- min(temp.df$time.hr)
  #temp.df <- temp.df %>% 
   # mutate(time.hr=time.hr-mintime) %>%
    #data.frame()
  
  #make linear model
  lm.Bsi <- lm(SiO2.prct~time.hr,temp.df)
  intercept <- coefficients(lm.Bsi) %>% .[1]
  slope <- coefficients(lm.Bsi) %>% .[2]
  R2 <- summary(lm.Bsi)$r.squared 
  p <- summary(lm.Bsi)$coefficients[2,4]
  mean.SiO2.prct <- mean(temp.df$SiO2.prct)
  
  temp.df$intercept <- intercept
  temp.df$slope <- slope
  temp.df$R2 <- R2
  temp.df$P <- p
  
  if(p<=0.05 & R2>=0.65){
    temp.df$extrapolate <- TRUE
    temp.df$SiO2.prct <- intercept
  } else {
    temp.df$extrapolate <- FALSE
    temp.df$SiO2.prct <- mean.SiO2.prct
  }
  
  temp.df <- temp.df %>%
    select(-c(time.hr)) %>%
    unique()
  
  results.list <- c(list(temp.df),results.list)
  
}

#combine results into a dataframe
results.df <- do.call("rbind",results.list)

silica.df <- results.df %>%
  select(c("location","depth.cm","SiO2.prct")) %>%
  group_by(location,depth.cm) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  drop_na(location)

save(silica.df,file="Rdata/silica.Rdata")
