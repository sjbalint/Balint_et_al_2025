rm(list=ls())

# load packages -----------------------------------------------------------

library(forecast) #for arima
library(zoo) #for rolling mean
library(tidyverse)

update_geom_defaults("point", list(shape = 21, fill="grey"))

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

minyear <- floor(min(data.df$year.mean, na.rm=TRUE))
maxyear <- ceiling(max(data.df$year.mean, na.rm=TRUE))

#make some data
data.df <- data.df %>%
  filter(outlier==FALSE) %>%
  drop_na(cluster) %>%
  arrange(year.mean) %>%
  drop_na(cluster, N.P.ratio)


# rolling mean ------------------------------------------------------------

result.list <- list()

k=20
dk=5

for (year in seq(minyear, (maxyear-k),dk)){
  
  temp.df <- data.df %>%
    filter(between(year.mean, year, year+k)) %>%
    summarize_if(is.numeric, mean, na.rm=TRUE)
  
  result.list <- c(result.list, list(temp.df))
}

mean.df <- bind_rows(result.list) %>%
  drop_na(year.mean)

# configure stats ---------------------------------------------------------

response.list <- c("sand.pct","mean.phi","sd.phi","accretion.rate.gcm2yr","%C.organic","%N","P.total.pct.e2",
                   "SiO2.prct","C.N.ratio","N.P.ratio","C.P.ratio","d15N.permil","d13C.organic")

# make arima model --------------------------------------------------------

result.list <- list()

for (response in response.list){
  
  input.df <- mean.df %>%
    select(c("year.mean", response))
  
  colnames(input.df) <- c("x","y")
  
  model <- auto.arima(input.df$y,
                      seasonal=FALSE,
                      stepwise=FALSE,
                      approximation=FALSE,
                      allowdrift=FALSE,
                      trace=TRUE)
  
  check.df <- checkresiduals(model, plot=FALSE)
  
  arima.df <- data.frame(name=response,
                         arima = model$fitted,
                         year.mean = input.df$x,
                         model=str_sub(check.df$data.name, start=16),
                         p.value=check.df$p.value) %>%
    mutate(p = str_sub(model,start=7,end=7),
           d = str_sub(model,start=9,end=9),
           q = str_sub(model,start=11,end=11),
           across(c("p","d","q"), as.numeric))
  
  result.list <- append(result.list, list(arima.df))
}

result.df <- bind_rows(result.list)

saveRDS(result.df, file="Rdata/arima.rds")
