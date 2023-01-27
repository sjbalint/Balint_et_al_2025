rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(knitr)
library(car)
library(dunn.test)

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")


# create a date (depth) threshold -----------------------------------------

threshold <- 20

for (row in 1:nrow(data.df)){
  ifelse(data.df[row,"depth.cm"]< threshold,
         data.df[row,"class"] <- "top",data.df[row,"class"] <- "bottom")
}


# configure stats ---------------------------------------------------------

alpha=0.05

predictor.list <- c("location","class")
response.list <- c("%C.organic","%N","P.total","SiO2.prct","CN","NP","SiP","d15N.permil","d13C.organic")

# test for normality ------------------------------------------------------

shapiro.df <- data.frame(matrix(ncol=0,nrow=length(response.list)))

for (row in 1:nrow(shapiro.df)){
  response <- response.list[row]
  shapiro.df[row,"response"] <- response
  shapiro.df[row,"shapiro.p.value"] <- shapiro.test(data.df[,response])$p.value
  shapiro.df[row,"shapiro.log.p.value"] <- shapiro.test(log(abs(data.df[,response])))$p.value
  if (shapiro.df[row,"shapiro.p.value"]>alpha){
    shapiro.df[row,"normal"] <- TRUE
  } else {
    shapiro.df[row,"normal"] <- FALSE
  }
  if (shapiro.df[row,"shapiro.log.p.value"]>alpha){
    shapiro.df[row,"lognormal"] <- TRUE
  } else {
    shapiro.df[row,"lognormal"] <- FALSE
  }
}

normal.list <- shapiro.df %>%
  filter(normal==TRUE) %>%
  select(response) %>%
  pull

abnormal.list <- shapiro.df %>%
  filter(normal==FALSE) %>%
  select(response) %>%
  pull

kable(shapiro.df)


# normal data -------------------------------------------------------------

if (length(normal.list)>0){
  
  response.list <- normal.list
  
  anova.df <- data.frame(matrix(ncol=0,nrow=length(response.list)*length(predictor.list)))
  
  row <- 0
  
  for (response in response.list){
    for (predictor in predictor.list){
      row <- row+1
      anova.df[row,"response"] <- response
      anova.df[row,"predictor"] <- predictor
      anova.df[row,"anova.p.value"] <- summary(aov(data.df[,response] ~ data.df[,predictor]))[[1]][1, 5]
      if (anova.df[row,"anova.p.value"]<alpha){
        anova.df[row,"anova.significance"] <- TRUE
      } else {
        anova.df[row,"anova.significance"] <- FALSE 
      }
    }
  }
  kable(anova.df) 
}


# post-hoc testing --------------------------------------------------------

if (length(normal.list)>0){
  tukey.df <- anova.df %>%
    filter(anova.significance==TRUE) %>%
    select(c("response","predictor"))
  
  tukey.results.df <- tukey.df
  
  for (row in 1:nrow(tukey.df)){
    response <- tukey.df[row,"response"]
    predictor <- tukey.df[row,"predictor"]
    thsd <- TukeyHSD(aov(data.df[,response] ~ data.df[,predictor]))
    temp.df <- data.frame(dimnames(thsd$`data.df[, predictor]`)[1], thsd$`data.df[, predictor]`[4])
    colnames(temp.df) <- c("comparison","tukey.p.value")
    temp.df$response <- tukey.df[row,"response"]
    temp.df$predictor <- tukey.df[row,"predictor"]
    tukey.results.df <- full_join(tukey.results.df,temp.df)
  }
  
  tukey.df <- tukey.results.df %>% 
    drop_na(tukey.p.value)
  
  for (row in 1:nrow(tukey.df)){
    if (tukey.df[row,"tukey.p.value"]<alpha/2){
      tukey.df[row,"tukey.significance"] <- TRUE
    } else {
      tukey.df[row,"tukey.significance"] <- FALSE
    }
  }
  
  tukey.results.df <- tukey.df %>%
    filter(tukey.significance==TRUE)
  
  kable(tukey.results.df)
}

# abnormal data -----------------------------------------------------------

if (length(abnormal.list)>0){
  
  response.list <- abnormal.list
  
  kruskal.df <- data.frame(matrix(ncol=0,nrow=length(response.list)*length(predictor.list)))
  
  row <- 0
  
  for (response in response.list){
    for (predictor in predictor.list){
      row <- row+1
      kruskal.df[row,"response"] <- response
      kruskal.df[row,"predictor"] <- predictor
      kruskal.df[row,"levene.p.value"] <- leveneTest(data.df[,response], data.df[,predictor])$`Pr(>F)`[1]
      
      if (kruskal.df[row,"levene.p.value"]>alpha){
        kruskal.df[row,"equal.variance"] <- TRUE
      } else {
        kruskal.df[row,"equal.variance"] <- FALSE
      }
      
      kruskal.df[row,"kruskal.p.value"] <- kruskal.test(data.df[,response],data.df[,predictor])$p.value
      if (kruskal.df[row,"kruskal.p.value"]<alpha){
        kruskal.df[row,"kruskal.significance"] <- TRUE
      } else {
        kruskal.df[row,"kruskal.significance"] <- FALSE 
      }
      if (kruskal.df[row,"equal.variance"]==FALSE){
        kruskal.df[row,"kruskal.p.value"] <- NA 
        kruskal.df[row,"kruskal.significance"] <- NA
      }
    }
  }
  
  
  kable(kruskal.df)
}


# post-hoc testing --------------------------------------------------------

if (length(abnormal.list)>0){
  dunn.df <- kruskal.df %>%
    filter(kruskal.significance==TRUE) %>%
    select(c("response","predictor"))
  
  dunn.results.df <- dunn.df
  
  for (row in 1:nrow(dunn.df)){
    response <- dunn.df[row,"response"]
    predictor <- dunn.df[row,"predictor"]
    dt <- dunn.test(data.df[,response],data.df[,predictor],method="bonferroni", 
                    kw=FALSE, table=FALSE)
    temp.df <- data.frame(dt$comparisons, dt$P.adjusted)
    colnames(temp.df) <- c("comparison","dunn.p.value")
    temp.df$response <- dunn.df[row,"response"]
    temp.df$predictor <- dunn.df[row,"predictor"]
    dunn.results.df <- full_join(dunn.results.df,temp.df)
  }
  
  dunn.df <- dunn.results.df %>% 
    drop_na(dunn.p.value)
  
  for (row in 1:nrow(dunn.df)){
    if (dunn.df[row,"dunn.p.value"]<alpha/2){
      dunn.df[row,"dunn.significance"] <- TRUE
    } else {
      dunn.df[row,"dunn.significance"] <- FALSE
    }
  }
  
  dunn.results.df <- dunn.df %>%
    filter(dunn.significance==TRUE)
  
  kable(dunn.results.df)
}


# regressions -------------------------------------------------------------

#response.list <- c(normal.list,abnormal.list)

