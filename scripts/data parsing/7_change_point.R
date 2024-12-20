rm(list=ls())

# load packages -----------------------------------------------------------

library(mcp)
library(changepoint)
library(segmented)
library(forecast) #for arima
library(vegan) #for permanova
library(tidyverse)

# import data -------------------------------------------------------------

data.df <- readRDS("Rdata/compiled_data.rds")

#clean up the data
data.df <- data.df %>%
  filter(outlier==FALSE)

response.list <- c("mean.phi","sd.phi","accretion.rate.gcm2yr","%C.organic","%N","P.total.pct.e2",
                   "SiO2.prct","C.N.ratio","N.P.ratio","C.P.ratio","d15N.permil","d13C.organic")


# first, test for differences between cores -------------------------------

#input data for permanova
perm.df <- data.df %>%
  select(c("location", response.list))

#scale data
perm.df[,2:ncol(perm.df)] <- scale(perm.df[,2:ncol(perm.df)])

#perform permanova
adonis2(perm.df[,2:ncol(perm.df)]~perm.df$location,
        method="euclidean", permutations = 9999, na.rm=TRUE)

# changepoint analysis ----------------------------------------------------

#lists to store results
model.list <- list()
changepoint.list <- list()

#iterate for every response
for (response in response.list){
  
  #subset data
  input.df <- data.df %>%
    select(c("year.mean", response)) %>%
    drop_na()
  
  #rename for model
  colnames(input.df) <- c("x","y")
  
  #calculate linear model
  lm_model <- lm(y ~ x, data = input.df)
  
  #calculate segmented model
  segmented_model <- selgmented(lm_model, seg.Z = ~ x, type="bic",
                                Kmax=5, msg=FALSE, bonferroni = TRUE)
  
  #get linear model p value
  lm.pval <- summary(lm_model)$coefficients[2,4]
  
  #perform anova between linear and segmented models
  anova.pval <- anova(lm_model,segmented_model)$`Pr(>F)`[2]
  
  #create dataframe of years for model prediction
  newdata <- data.frame(x=seq(min(input.df$x),max(input.df$x),1))
  
  #get prediction from linear model with 95% confidence interval
  prediction.df <- predict(segmented_model, newdata, se.fit = TRUE, 
                           interval = "confidence", level = 0.95)$fit %>%
    as.data.frame()
  
  #rename columns of prediction
  colnames(prediction.df) <- c("fit","fit.upper","fit.lower")
  
  #add year to prediction dataframe
  prediction.df$year.mean <- newdata$x
  
  #make a second dataframe of the fitted values
  model.df <- data.frame(year.mean=input.df$x,
                         mean.value=input.df$y,
                         model=segmented_model$fitted.values,
                         residuals=segmented_model$residuals)
  
  #bind everything together and add p values
  model.df <- bind_rows(model.df,prediction.df) %>%
    mutate(name=response,
           lm.pval=lm.pval,
           anova.pval=anova.pval)
  
  #add dataframe to list of results
  model.list <- c(model.list, list(model.df))
  
  #extract changepoints from segmented model
  changepoint.df <- confint(segmented_model) %>%
    as.data.frame()
  
  #extract beginning and end of segmented model
  model_ends <- data.frame(name=response,
                           year.mean=c(min(input.df$x), max(input.df$x)))
  
  #if there is a breakpoint
  if (ncol(changepoint.df)>2){
    colnames(changepoint.df) <- c("year.mean", "year.lower", "year.upper")
    
    rownames(changepoint.df) <- NULL
    
    changepoint.df <- changepoint.df %>%
      mutate(breakpoint = row_number(),
             name=response) %>%
      select(breakpoint, name, everything())
    
    changepoint.df <- bind_rows(changepoint.df, model_ends)
    
  } else {
    
    #if no changepoint, just return the top and bottom of core
    changepoint.df <- model_ends
    
  }
  
  #predict model result at each changepoint
  newdata <- data.frame(x=changepoint.df$year.mean)
    
  prediction.df <- predict(segmented_model, newdata,
                           se.fit = TRUE, interval = "confidence", level = 0.95)$fit %>%
    as.data.frame()
    
  colnames(prediction.df) <- c("fit","fit.lower","fit.upper")
  
  changepoint.df <- bind_cols(changepoint.df, prediction.df) %>%
    mutate(CI.95 = (fit.upper-fit.lower)/2,
           lm.pval=lm.pval,
           anova.pval=anova.pval,
           across(c("fit","fit.upper","fit.lower","CI.95"), signif, digits=3))
  
  #return the modelled results at each changepoint
  changepoint.list <- c(changepoint.list, list(changepoint.df)) 
  
}

# save data ---------------------------------------------------------------

#compile dataframes
model.df <- bind_rows(model.list)
changepoint.df <- bind_rows(changepoint.list)

saveRDS(model.df, file="Rdata/segmented_model.rds")
saveRDS(changepoint.df, file="Rdata/segmented_changepoints.rds")

write.csv(changepoint.df, file="output/changepoints.csv")


