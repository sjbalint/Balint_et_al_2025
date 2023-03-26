rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)

# import data -------------------------------------------------------------

north.df <- read_excel("raw/dating/US-EPA-WHETS_Pb-Cs_Models_rplum_23.2.8.xlsx", sheet=2) %>%
  mutate(location="North")

middle.df <- read_excel("raw/dating/US-EPA-WHETS_Pb-Cs_Models_rplum_23.2.8.xlsx", sheet=3) %>%
  mutate(location="Middle")

south.df <- read_excel("raw/dating/US-EPA-WHETS_Pb-Cs_Models_rplum_23.2.8.xlsx", sheet=4) %>%
  mutate(location="South")

dating.df <- bind_rows(north.df,middle.df,south.df)

colnames(dating.df) <- c("depth.cm","year.min","year.max","year.median","year.mean","end.pb","cs.peak","location")


# correct for dates -------------------------------------------------------



date_shift <- TRUE

if (date_shift==TRUE){
  for (row in 1:nrow(dating.df)){
    if (dating.df[row,"location"]=="South"){
      dating.df[row,"depth.cm"] <- dating.df[row,"depth.cm"]-10
    }
    if (dating.df[row,"location"]=="North"){
      dating.df[row,"depth.cm"] <- dating.df[row,"depth.cm"]-0
    }
  }
}

location.list <- dating.df %>%
  pull(location) %>%
  unique()

for (location in location.list){
  for (row in 2:nrow(dating.df)){
    if (dating.df[row-1,"location"]==location & dating.df[row,"location"]==location){
      dating.df[row,"n.years"] <- dating.df[row-1,"year.mean"]-dating.df[row,"year.mean"]
      dating.df[row,"n.cm"] <- dating.df[row,"depth.cm"]-dating.df[row-1,"depth.cm"]
      dating.df[row,"accretion.rate.cmyr"] <- dating.df[row,"n.cm"]/dating.df[row,"n.years"] 
    }
  } 
}

dating.df$century <- factor(round(dating.df$year.mean/100)*100) #deturmine century for stats

save(dating.df,file="Rdata/dating.Rdata")
