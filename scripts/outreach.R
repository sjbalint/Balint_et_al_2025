rm(list = ls()) #clear environment

# import packages and data ------------------------------------------------

library(tidyverse)

# generate random seawater data -------------------------------------------

targets <- data.frame(
  c(0.66,0.33),
  c("Narragansett Bay","Academy Cove"),
  c(0.04, 0.5),
  c(0.15,1.5),
  c(400,100)
)

colnames(targets) <- c("fraction","location","target.N","target.C","volume")

n_samples <- 60
rsd <- 0.3

POM <- data.frame()

for (mylocation in targets$location){
  mysubset <- subset(targets,location==mylocation)
  n <- rnorm(n=1,mean=n_samples*mysubset$fraction)
  target_mgN <- mysubset$target.N/(1000/mysubset$volume)
  target_mgC <- mysubset$target.C/(1000/mysubset$volume)
  target_CN <- target_mgC/target_mgN
  POM_mgN <- rnorm(n=n,mean=target_mgN,sd=target_mgN*rsd)
  CN <- rnorm(n=n,mean=target_CN,sd=target_CN*rsd/2)
  POM_mgC <- POM_mgN*CN
  temp <- data.frame(POM_mgN,POM_mgC)
  temp$location <- mylocation
  temp$volume <- mysubset$volume
  if (mylocation=="Academy Cove"){
    temp$year <- 2022
  } else {
    for (row in 1:nrow(temp)){
      seed <- rnorm(1,mean=1)
      if (seed >=1){
        temp[row,"year"] <- 2022
      } else {
        temp[row,"year"] <- 2017
      }
    }
  }
  if (nrow(POM)==0){
    POM <- temp
  } else {
    POM <- rbind(POM,temp)
  }
}

POM <- POM[sample(1:nrow(POM)),c("year","location","POM_mgN","POM_mgC","volume")]

write.csv(POM,"data/POM_export.csv")

# generate random seaweed data --------------------------------------------

targets <- data.frame(
  c("Green","Brown","Red"), #colors
  c(0.4,0.35,0.35), #fraction of samples
  c(2.8,2.5,2.3), #%N
  c(39,38,39) #%C
  )
colnames(targets) <- c("color","fraction","target.N","target.C")

target_weight <- 3 #mg
rsd <- 0.1

seaweed <- data.frame()

for (mycolor in targets$color){
  mysubset <- subset(targets,color==mycolor)
  n <- rnorm(n=1,mean=n_samples*mysubset$fraction)
  target_mgN <- mysubset$target.N*target_weight/100
  target_mgC <- mysubset$target.C*target_weight/100
  target_CN <- target_mgC/target_mgN
  seaweed_mgN <- rnorm(n=n,mean=target_mgN,sd=target_mgN*rsd)
  CN <- rnorm(n=n,mean=target_CN,sd=target_CN*rsd/2)
  seaweed_mgC <- seaweed_mgN*CN
  seaweed_weight <- rnorm(n=n,mean=target_weight,sd=target_weight*rsd/2)
  temp <- data.frame(seaweed_mgN,seaweed_mgC,seaweed_weight)
  temp$color <- mycolor
  if (nrow(seaweed)==0){
    seaweed <- temp
  } else {
    seaweed <- rbind(seaweed,temp)
  }
}

seaweed <- seaweed[sample(1:nrow(seaweed)),c("color","seaweed_mgN","seaweed_mgC","seaweed_weight")]

write.csv(seaweed,"data/seaweed_export.csv")

# generate sediment data --------------------------------------------------

targets <- data.frame(
  c("Under Water","Mid Beach","Upper Beach"), #colors
  c(0.5,0.4,0.1), #fraction of samples
  c(0.06,0.04,0.01), #%N
  c(5,5,5) #%C
)
colnames(targets) <- c("location","fraction","target.N","target.C")

target_weight <- 20 #mg
rsd <- 0.1

sediment <- data.frame()

for (mylocation in targets$location){
  mysubset <- subset(targets,location==mylocation)
  n <- rnorm(n=1,mean=n_samples*mysubset$fraction)
  target_mgN <- mysubset$target.N*target_weight/100
  target_mgC <- mysubset$target.C*target_weight/100
  sediment_mgN <- rnorm(n=n,mean=target_mgN,sd=target_mgN*rsd*2)
  CN <- rnorm(n=n,mean=target_CN,sd=target_CN*rsd/2)
  sediment_mgC <- sediment_mgN*CN
  sediment_weight <- rnorm(n=n,mean=target_weight,sd=target_weight*rsd/2)
  temp <- data.frame(sediment_mgN,sediment_mgC,sediment_weight)
  temp$location <- mylocation
  if (nrow(sediment)==0){
    sediment <- temp
  } else {
    sediment <- rbind(sediment,temp)
  }
}

sediment <- sediment[sample(1:nrow(sediment)),c("location","sediment_mgN","sediment_mgC","sediment_weight")]

write.csv(sediment,"data/sediment_export.csv")
