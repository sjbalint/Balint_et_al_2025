rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)

dat <- read.csv("C:/Users/Melissa Hagy/CCASE/CCASE_SedimentSi.csv")

#separate data into levels based on sample ID
lvl_ID<- unique(dat$ID)

#Create a dataframe for the p-value, R2, slope, intercept. 
#Start by making vectors for each column.

#taking unique ID's and put in a dataframe to automatically dimension the dataframe 
#so it doesn't matter how many samples
#and create empty variables for p, r2 and intercept
CCASE_BSi <- data.frame(lvl_ID) %>% 
  mutate(
    p=NA,R2=NA,intercept=NA,slope=NA,averageBsi=NA
  )

#for loop
for ( i in 1:nrow(CCASE_BSi) ){
  #create a subset data 
  data_sub <- filter(dat,ID== CCASE_BSi$lvl_ID[i])
  #recode time variable to always start with 0 so intercept is calulated back to first time point
  mintime <- min(data_sub$Time)
  data_sub <- data_sub %>% mutate(Time=Time-mintime)
  
  #create the linear model. If it is the first loop,
  #then the model name will be lm1
  lm.Bsi <- lm(Wtperc_Bsi~Time,data=data_sub)
  intercept <- coefficients(lm.Bsi) %>% .[1]
  slope <- coefficients(lm.Bsi) %>% .[2]
  R2 <- summary(lm.Bsi)$r.squared 
  p <- summary(lm.Bsi)$coefficients[2,4]
  averageBsi <- mean(data_sub$Wtperc_Bsi)
  CCASE_BSi[i, 2:6] <- c(p, R2, intercept,slope,averageBsi)
  
}
#calculate BSi based on (p ??? 0.05, R2 ??? 0.65) less than extrapolate to intercept otherwise average
#by creating true or false (logical) variable
CCASE_BSi <- CCASE_BSi %>% 
  mutate(extrapolate=(p<=0.1 & R2>=0.65 & slope >0 ))
#If conditions for extrapolation are met, extrapolate to intercept
CCASE_BSi$Bsi[CCASE_BSi$extrapolate] <- CCASE_BSi$intercept[CCASE_BSi$extrapolate]
#! means not or negation (no extrapolate use averae)
CCASE_BSi$Bsi[!CCASE_BSi$extrapolate] <- CCASE_BSi$averageBsi[!CCASE_BSi$extrapolate]
# 
# pdf(file="C:/Users/Melissa Hagy/CCASE/CCASESigraphs_facet.pdf",width=7,height=10,useDingbats = FALSE,paper="letter")
# 
# ggplot(dat,aes(x=Time,y=Wtperc_Bsi))+
#   geom_point()+
#   facet_wrap(~ID)+
#   geom_smooth(method="lm")
# 
# dev.off()

pdf(file="C:/Users/Melissa Hagy/CCASE/CCASESigraphs.pdf",width=4,height=4,useDingbats = FALSE,paper="letter")
for (i in 1:nrow(CCASE_BSi)) {
  lbl <- paste("R2=",round(CCASE_BSi$R2[i],2),"p=",round(CCASE_BSi$p[i],2),
               "slope=",round(CCASE_BSi$slope[i],2))
  data_sub <- subset(dat,ID== CCASE_BSi$lvl_ID[i])
  plt <- ggplot(data=data_sub,aes(x=Time,y=Wtperc_Bsi))+
    geom_point()+
    theme_classic()+
    geom_smooth(method="lm")+
    ggtitle(paste("BSi Concentration, ID=",lvl_ID[i]),subtitle=lbl)
  print(plt)
}
dev.off()

#Export the data into a CSV
save(CCASE_BSi,file="Rdata/BSi.Rdata")

