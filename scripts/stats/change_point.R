rm(list=ls())

# load packages -----------------------------------------------------------

library(mcp)
library(changepoint)
library(segmented)
library(forecast) #for arima
library(vegan) #for permanova
library(tidyverse)

update_geom_defaults("point", list(shape = 21, fill="grey"))

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

#clean up the data
data.df <- data.df %>%
  filter(outlier==FALSE) %>%
  drop_na(cluster) %>%
  arrange(year.mean) %>%
  drop_na(cluster, N.P.ratio)

response.list <- c("sand.pct","mean.phi","sd.phi","accretion.rate.gcm2yr","%C.organic","%N","P.total.pct.e2",
                   "SiO2.prct","C.N.ratio","N.P.ratio","C.P.ratio","d15N.permil","d13C.organic")


# first, test for differences between cores -------------------------------

#input data for permanova
perm.df <- data.df %>%
  select(c("location", response.list))

perm.df[,2:14] <- scale(perm.df[,2:14])

#perform permanova
adonis2(perm.df[,2:14]~perm.df$location, method="euclidean", permutations = 99999)

# changepoint analysis ----------------------------------------------------

model.list <- list()
changepoint.list <- list()

response <- "sand.pct"

for (response in response.list){
  
  input.df <- data.df %>%
    select(c("year.mean", response))
  
  colnames(input.df) <- c("x","y")
  
  lm_model <- lm(y ~ x, data = input.df)
  
  segmented_model <- selgmented(lm_model, seg.Z = ~ x, type="bic",
                                Kmax=5, msg=FALSE, bonferroni = TRUE)
  
  summary <- summary(segmented_model)$coefficients
  
  lm.pval <- summary(lm_model)$coefficients[2,4]
  anova.pval <- anova(lm_model,segmented_model)$`Pr(>F)`[2]
  
  years.df <- data.frame(x=seq(min(input.df$x),max(input.df$x),1))
  
  prediction.df <- predict(segmented_model, years.df, se.fit = TRUE, interval = "confidence", level = 0.95)$fit %>%
    as.data.frame()
  
  colnames(prediction.df) <- c("fit","fit.upper","fit.lower")
  
  prediction.df$year.mean <- years.df$x
  
  model.df <- data.frame(year.mean=input.df$x,
                         mean.value=input.df$y,
                         model=segmented_model$fitted.values,
                         residuals=segmented_model$residuals)
  
  model.df <- bind_rows(model.df,prediction.df) %>%
    mutate(name=response,
           lm.pval=lm.pval,
           anova.pval=anova.pval)
  
  model.list <- c(model.list, list(model.df))
  
  changepoint.df <- confint(segmented_model) %>%
    as.data.frame()
  
  model_ends <- data.frame(name=response,
                           year.mean=c(min(input.df$x), max(input.df$x)))
  
  if (ncol(changepoint.df)>2){
    colnames(changepoint.df) <- c("year.mean", "year.lower", "year.upper")
    
    rownames(changepoint.df) <- NULL
    
    changepoint.df <- changepoint.df %>%
      mutate(breakpoint = row_number(),
             name=response) %>%
      select(breakpoint, name, everything())
    
    changepoint.df <- bind_rows(changepoint.df, model_ends)
    
  } else {
    
    changepoint.df <- model_ends
    
  } #else
    
    years.df <- data.frame(x=changepoint.df$year.mean)
    
    prediction.df <- predict(segmented_model, years.df,
                             se.fit = TRUE, interval = "confidence", level = 0.95)$fit %>%
      as.data.frame()
    
    colnames(prediction.df) <- c("fit","fit.lower","fit.upper")
    
    changepoint.df <- bind_cols(changepoint.df, prediction.df) %>%
      mutate(CI.95 = (fit.upper-fit.lower)/2,
             lm.pval=lm.pval,
             anova.pval=anova.pval,
             across(c("fit","fit.upper","fit.lower","CI.95"), signif, digits=3))
    
    changepoint.list <- c(changepoint.list, list(changepoint.df)) 
  
}

model.df <- bind_rows(model.list)
changepoint.df <- bind_rows(changepoint.list)

saveRDS(model.df, file="Rdata/segmented_model.rds")
saveRDS(changepoint.df, file="Rdata/segmented_changepoints.rds")

write.csv(changepoint.df, file="output/changepoints.csv")


# plot residuals ----------------------------------------------------------

ggplot(model.df, aes(year.mean, residuals))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point()+
  facet_wrap(.~name, scales="free")


