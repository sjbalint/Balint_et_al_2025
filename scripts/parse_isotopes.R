rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)

# import data -------------------------------------------------------------

load("Rdata/silica.Rdata")

dates.df <- read_csv("raw/core_dating_2.csv", show_col_types=FALSE) %>%
  mutate_if(is.character,as.factor)

dates.df$date.depth.cm <- dates.df$depth.cm

mydata.df <- read_excel("raw/isotopes_09072022.xlsx") %>%
  filter(ignore==FALSE) %>%
  select(c("location","depth.cm","sample.type","%N","d15N.permil","%C","d13C.permil")) %>%
  mutate(across(all_of(c("location","sample.type")),as.factor))

P.df <- read_excel("raw/data_08232022.xlsx",sheet="phosphorus")


# caluclate n and means ---------------------------------------------------

n.df <- mydata.df %>%
  filter(sample.type=="Acidified") %>%
  group_by(location,sample.type,depth.cm) %>%
  count() %>%
  ungroup() %>%
  select(-sample.type)

mean.df <- mydata.df %>%
  group_by(location,sample.type,depth.cm) %>%
  summarize_all(mean,na.rm=TRUE) %>%
  ungroup()


# split acidified and unacidified -----------------------------------------


unacidified.df <- mean.df %>%
  filter(sample.type=="Unacidified") %>%
  rename("d13C.total"="d13C.permil",
         "%C.total"="%C") %>%
  select(-sample.type)

acidified.df <- mean.df %>%
  filter(sample.type=="Acidified") %>%
  rename("d13C.organic"="d13C.permil",
         "%C.organic"="%C") %>%
  select(-c('sample.type',"%N","d15N.permil"))

# combine data ------------------------------------------------------------


iso.df <- full_join(unacidified.df,acidified.df)

iso.df <- left_join(iso.df,n.df)

iso.df <- left_join(iso.df,P.df)

iso.df <- left_join(iso.df,silica.df)


# final calculations ------------------------------------------------------

iso.df$NP <- iso.df$`%N`/iso.df$P.total

iso.df$CN <- iso.df$`%C.total`/iso.df$`%N`

iso.df$P.total <- iso.df$P.total*100

iso.df$location <- factor(iso.df$location,levels=c("North","Middle","South"))

iso.df <- left_join(iso.df,dates.df)


# date shift --------------------------------------------------------------

date_shift <- TRUE

if (date_shift==TRUE){
  for (row in 1:nrow(iso.df)){
    if (iso.df[row,"location"]=="South"){
      iso.df[row,"depth.cm"] <- iso.df[row,"depth.cm"]+12
      iso.df[row,"date.depth.cm"] <- iso.df[row,"date.depth.cm"]+12
    }
    if (iso.df[row,"location"]=="North"){
      iso.df[row,"depth.cm"] <- iso.df[row,"depth.cm"]+4
      iso.df[row,"date.depth.cm"] <- iso.df[row,"date.depth.cm"]+4
    }
  }
}


# export ------------------------------------------------------------------



save(iso.df,file="Rdata/iso.Rdata")

 