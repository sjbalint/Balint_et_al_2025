rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)


# import data -------------------------------------------------------------

load("Rdata/silica.Rdata")

load("Rdata/isotopes.Rdata")

load("Rdata/dating.Rdata")

load("Rdata/grainsize.Rdata")

P.df <- read.csv("raw/phosphorus.csv")

bulk.df <- read.csv("raw/bulk_density.csv")


# summarize grainsize data ------------------------------------------------

grain.df <- grain.df %>%
  select(location,depth.cm,class.pct,class.rough) %>%
  group_by(location,depth.cm,class.rough) %>%
  summarize_all(sum) %>%
  ungroup() %>%
  pivot_wider(names_from=class.rough,values_from=class.pct)

colnames(grain.df) <- c("location","depth.cm","clay.pct","gravel.pct","sand.pct")


# combine data ------------------------------------------------------------

data.df <- left_join(isotopes.df,P.df) %>%
  select(-n)

data.df <- left_join(data.df,silica.df)

data.df <- left_join(data.df,bulk.df)

data.df <- left_join(data.df,grain.df)

data.df <- left_join(data.df,dating.df)


# final calculations ------------------------------------------------------

data.df <- data.df %>%
  mutate(
    N.P.ratio=`%N`/P.pct.total,
    C.N.ratio=`%C.total`/`%N`,
    Si.P.ratio=SiO2.prct/P.pct.total,
    Si.N.ratio=SiO2.prct/`%N`,
    P.total.pct.e2 = P.pct.total*100
  )

data.df["Si.P.ratio"][data.df["Si.P.ratio"]<0] <- NA

data.df <- data.df %>%
  mutate(location=factor(location,levels=c("North","Middle","South"))) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.integer,as.numeric) %>%
  as.data.frame


# date shift --------------------------------------------------------------

date_shift <- FALSE

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
  mutate(N.storage=0.24*bulk.density.gcm3*`%N`,
         median.grainsize.phi=log2(median.grainsize.um),
         accretion.rate.gcm2yr=accretion.rate.cmyr*bulk.density.gcm3)

# export data -------------------------------------------------------------

save(data.df,file="Rdata/compiled_data.Rdata")

write.csv(data.df,"output/compiled_data_BALINT_03262023.csv",row.names=FALSE)
