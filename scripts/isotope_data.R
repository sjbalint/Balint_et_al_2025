rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)

# import data -------------------------------------------------------------

load("Rdata/dates.Rdata")

mydata.df <- read_excel("data/isotopes_09072022.xlsx") %>%
  filter(ignore==FALSE) %>%
  select(c("location","depth.cm","sample.type","%N","d15N.permil","%C","d13C.permil")) %>%
  mutate(across(all_of(c("location","sample.type")),as.factor))


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


# export data -------------------------------------------------------------

export.df <- full_join(unacidified.df,acidified.df)

export.df <- left_join(export.df,n.df)

write.csv(export.df,"data/export/isotopes.csv")

iso.df <- read_excel("data/data_08232022.xlsx",sheet="isotopes") %>%
  mutate(across(location,as.factor))%>%
  mutate_if(is.character,as.numeric)

P.df <- read_excel("data/data_08232022.xlsx",sheet="phosphorus")

iso.df <- left_join(iso.df,P.df)

iso.df$NP <- iso.df$`%N`/iso.df$P.total

iso.df$CN <- iso.df$`%C.total`/iso.df$`%N`

iso.df$P.total <- iso.df$P.total*100

iso.df$location <- factor(iso.df$location,levels=c("North","Middle","South"))

iso.df <- left_join(iso.df,dates.df)

save(iso.df,file="Rdata/iso.Rdata")

 