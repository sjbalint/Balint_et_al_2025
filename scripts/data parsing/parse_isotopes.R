rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)

# import data -------------------------------------------------------------

rawdata.df <- read_excel("raw/isotopes/irms_master.xlsx",sheet="Compiled") %>%
  filter(ignore==FALSE) %>%
  select(c("location","depth.cm","sample.type","%N","d15N.permil","%C","d13C.permil")) %>%
  mutate(across(all_of(c("location","sample.type")),as.factor))


# caluclate n and means ---------------------------------------------------

n.df <- rawdata.df %>%
  filter(sample.type=="Acidified") %>%
  group_by(location,sample.type,depth.cm) %>%
  count() %>%
  ungroup() %>%
  select(-sample.type)

mean.df <- rawdata.df %>%
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


isotopes.df <- full_join(unacidified.df,acidified.df)
isotopes.df <- left_join(isotopes.df,n.df)


# export data -------------------------------------------------------------

save(isotopes.df,file="Rdata/isotopes.Rdata")

 