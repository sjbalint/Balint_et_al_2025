rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)


# import data -------------------------------------------------------------

load("Rdata/silica.Rdata")

load("Rdata/isotopes.Rdata")

load("Rdata/dating.Rdata")

load("Rdata/grainsize.Rdata")

load("Rdata/grainstats.Rdata")

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

data.df <- full_join(isotopes.df,P.df) %>%
  select(-n)

data.df <- full_join(data.df,silica.df)

data.df <- full_join(data.df,bulk.df)

data.df <- full_join(data.df,grain.df)

data.df <- full_join(data.df,grainstats.df)

data.df <- left_join(data.df,dating.df)


# final calculations ------------------------------------------------------

data.df["P.pct.total"][data.df["P.pct.total"]<0] <- NA

data.df <- data.df %>%
  mutate(
    N.P.ratio=`%N`/P.pct.total,
    C.N.ratio=`%C.total`/`%N`,
    Si.P.ratio=SiO2.prct/P.pct.total,
    Si.N.ratio=SiO2.prct/`%N`,
    P.total.pct.e2 = P.pct.total*100
  )

data.df <- data.df %>%
  mutate(location=factor(location,levels=c("North","Middle","South"))) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.integer,as.numeric) %>%
  as.data.frame

# N storage ---------------------------------------------------------------

data.df <- data.df %>%
  mutate(N.storage=0.24*bulk.density.gcm3*`%N`,
         median.grainsize.phi=log2(median.grainsize.um),
         accretion.rate.gcm2yr=accretion.rate.cmyr*bulk.density.gcm3)


# denote outliers ---------------------------------------------------------

data.df <- data.df %>%
  mutate(outlier=ifelse(location=="South" & depth.cm>38,TRUE,FALSE),
         outlier=ifelse(depth.cm==0,TRUE,outlier))

data.df$century <- factor(round(data.df$year.mean/100)*100) #determine century for stats

# export data -------------------------------------------------------------

save(data.df,file="Rdata/compiled_data.Rdata")

write.csv(data.df,"output/compiled_data_BALINT.csv",row.names=FALSE)
