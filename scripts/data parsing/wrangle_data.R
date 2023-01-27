rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)


# import data -------------------------------------------------------------

load("Rdata/silica.Rdata")

load("Rdata/isotopes.Rdata")

dates.df <- read.csv("raw/dating/core_dating_2.csv")

dates.df$date.depth.cm <- dates.df$depth.cm

P.df <- read.csv("raw/phosphorus.csv")

bulk.df <- read.csv("raw/bulk_density.csv")


# combine data ------------------------------------------------------------

data.df <- left_join(isotopes.df,P.df)

data.df <- left_join(data.df,silica.df)

data.df <- left_join(data.df,bulk.df)


# final calculations ------------------------------------------------------

data.df$NP <- data.df$`%N`/data.df$P.pct.total

data.df$CN <- data.df$`%C.total`/data.df$`%N`

data.df$SiP <- data.df$SiO2.prct/data.df$P.pct.total

data.df["SiP"][data.df["SiP"]<0] <- NA

data.df$P.total <- data.df$P.pct.total*100

data.df <- left_join(data.df,dates.df)

data.df <- data.df %>%
  mutate(location=factor(location,levels=c("North","Middle","South")),
         type=as.factor(type)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.integer,as.numeric) %>%
  as.data.frame


# date shift --------------------------------------------------------------

date_shift <- TRUE

if (date_shift==TRUE){
  for (row in 1:nrow(data.df)){
    if (data.df[row,"location"]=="South"){
      data.df[row,"depth.cm"] <- data.df[row,"depth.cm"]+12
      data.df[row,"date.depth.cm"] <- data.df[row,"date.depth.cm"]+12
    }
    if (data.df[row,"location"]=="North"){
      data.df[row,"depth.cm"] <- data.df[row,"depth.cm"]+4
      data.df[row,"date.depth.cm"] <- data.df[row,"date.depth.cm"]+4
    }
  }
}


# N storage ---------------------------------------------------------------

data.df <- data.df %>%
  mutate(N.storage=0.24*bulk.density.g.cm*`%N`)


# export data -------------------------------------------------------------

save(data.df,file="Rdata/compiled_data.Rdata")
