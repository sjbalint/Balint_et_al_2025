rm(list=ls())

# load packages -----------------------------------------------------------

library(tidyverse)

# import data -------------------------------------------------------------

data.df <- readRDS("Rdata/compiled_data.rds")

# summarize data ----------------------------------------------------------

response.list <- c("mean.phi","sd.phi","accretion.rate.gcm2yr","%C.organic","%N","P.total.pct.e2",
                   "SiO2.prct","C.N.ratio","N.P.ratio","C.P.ratio","d15N.permil","d13C.organic")

data.df <- data.df %>%
  mutate(location=ifelse(outlier==TRUE, "Outlier",as.character(location)),
         location=factor(location, levels=c("North","Middle","South","Outlier"))) %>%
  drop_na(location) %>%
  group_by(location) %>%
  select(all_of(response.list)) %>%
  summarize_all(mean, na.rm=TRUE) %>%
  ungroup() %>%
  mutate_if(is.numeric, signif, digits=3)

# export the table --------------------------------------------------------

write.csv(data.df, file="output/table2.csv", row.names=FALSE)  
