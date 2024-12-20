rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)

# import data -------------------------------------------------------------

silica.df <- readRDS("Rdata/silica.rds")

isotopes.df <- readRDS("Rdata/isotopes.rds")

dating.df <- readRDS("Rdata/dating.rds") %>%
  select(location, depth.cm, thickness.cm, accretion.rate.cmyr, year.mean)

grainstats.df <- readRDS("Rdata/grainstats.rds")

P.df <- read.csv("raw/phosphorus.csv")

bulk.df <- read.csv("raw/bulk_density.csv")

# combine data ------------------------------------------------------------

data.df <- full_join(isotopes.df,P.df) %>%
  select(-n)

data.df <- full_join(data.df,silica.df)

data.df <- full_join(data.df,bulk.df)

data.df <- full_join(data.df,grainstats.df)

# add dating, including 10cm of erosion -----------------------------------

data.df <- data.df %>%
  mutate(depth.cm=ifelse(location=="South",
         depth.cm+10,
         depth.cm))

data.df <- left_join(data.df,dating.df)

endpb.df <- data.frame(location=as.factor(c("North","Middle","South")),
                       end.pb = c(38,38,42))

data.df <- left_join(data.df, endpb.df) %>%
  mutate(below_pb_LOD=ifelse(depth.cm>end.pb, TRUE,FALSE))

# final calculations ------------------------------------------------------

#tidy the data
data.df <- data.df %>%
  mutate(location=factor(location,levels=c("North","Middle","South"))) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.integer,as.numeric) %>%
  as.data.frame() %>%
  arrange(location, depth.cm)

#calculate elemental ratios
data.df <- data.df %>%
  mutate(
    P.pct.total = ifelse(P.pct.total<0,NA,P.pct.total),
    N.P.ratio=`%N`/P.pct.total,
    C.N.ratio=`%C.organic`/`%N`,
    Si.P.ratio=SiO2.prct/P.pct.total,
    Si.N.ratio=SiO2.prct/`%N`,
    C.P.ratio=`%C.organic`/`P.pct.total`,
    P.total.pct.e2 = P.pct.total*100,
    accretion.rate.gcm2yr=accretion.rate.cmyr*bulk.density.gcm3
  )


# outlier detection -------------------------------------------------------

response.list <- c("%C.organic","%N", "P.total.pct.e2")

long.df <- data.df %>%
  select(all_of(c("location","depth.cm",response.list))) %>%
  pivot_longer(all_of(response.list))

result.list <- list()

for (response in response.list){
  
  temp.df <- long.df %>%
    filter(name==response)
  Q <- quantile(temp.df$value, probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(temp.df$value,na.rm = TRUE)
  upper <- Q[2]+3*iqr
  lower <- Q[1]-3*iqr
  
  result.df <- data.frame("name"=response,
                          "upper"=upper,
                          "lower"=lower)
  
  result.list <- c(result.list, list(result.df))
}

result.df <- bind_rows(result.list)

outlier.df <- left_join(long.df, result.df) %>%
  mutate(outlier=ifelse(value>upper,1,0),
         outlier=ifelse(value<lower,1,outlier)) %>% 
  group_by(location, depth.cm) %>%
  summarize(outlier=sum(outlier)) %>%
  ungroup() %>%
  mutate(outlier=ifelse(outlier>0, TRUE, FALSE))

data.df <- left_join(data.df, outlier.df)

# export data -------------------------------------------------------------

saveRDS(data.df,file="Rdata/compiled_data.rds")

write.csv(data.df,"output/compiled_data.csv",row.names=FALSE)
