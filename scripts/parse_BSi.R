rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)

# import data -------------------------------------------------------------

filepath <- "raw/silica/01132023/"
#filepath <- "raw/silica/12092022/"

weights.df <- read_excel(paste0(filepath,"BSi_weights.xlsx")) #weights

#create a function to read each excel file
read_SEAL <- function(filename){
  print(paste("Importing",filename)) #print text so we can see what's happening
  df <- read.csv(filename,skip=12) %>%
  return(df) #return a dataframe
}

seal.df <- #new dataframe of our raw data
  list.files(path = paste0(filepath,"seal/"),full.names=TRUE) %>% #list the filenames of every exceul document oin the folder
  map_df(~read_SEAL(.)) %>% #create a big dataframe using our function
  select((c("Sample.ID","Peak.Number","Cup.Type","Results.5","AD.Values")))

colnames(seal.df) <- c("Sample.ID","peak","type","SiO2.uM","ad.values")

seal.df["SiO2.uM"][seal.df["SiO2.uM"] == "0"] <- NA

seal.df <- seal.df %>%
  mutate(across(c("type"),as.factor)) %>%
  mutate(across(c("peak","SiO2.uM","ad.values"),as.numeric)) %>%
  group_by(Sample.ID) %>%
  summarize_all(mean,na.rm=TRUE) %>%
  ungroup()

str(seal.df) #print the first 6 lines of our dataframe


# parse sample ID ---------------------------------------------------------

seal.df$id <- str_sub(seal.df$Sample.ID,0,-2) %>% #return numeric sample identifier
  as.numeric()

seal.df$time <- str_sub(seal.df$Sample.ID,start=-1) #return letter time indentifier

#convert letter time indentifier to number of hours
seal.df$time.hr <- factor(seal.df$time,levels=c("A","B","C"), labels=c(3,4,5)) %>%
  as.character() %>%
  as.numeric()

silica.df <- left_join(seal.df,weights.df)
  #drop_na(replicate)


# calculate percent SiO ---------------------------------------------------

silica.df$SiO2.mg <- silica.df$SiO2.uM * 60.08 * 0.04 * 10 * 0.1

silica.df$SiO2.prct <- silica.df$SiO2.mg/silica.df$final.mg

silica.df <- silica.df %>%
  select(c("id","time.hr","replicate","location","depth.cm","SiO2.mg","SiO2.prct"))


# linear regression -------------------------------------------------------

id.list <- silica.df %>% #create list of sample ids
  drop_na(c("id","depth.cm","SiO2.prct")) %>%
  pull(id) %>%
  unique()

results.list <- list()

for (myid in id.list){
  temp.df <- silica.df %>%
    filter(id==myid)
  #recode time variable to always start with 0 so intercept is calculated back to first time point
  #mintime <- min(temp.df$time.hr)
  #temp.df <- temp.df %>% 
   # mutate(time.hr=time.hr-mintime) %>%
    #data.frame()
  
  #make linear model
  lm.Bsi <- lm(SiO2.prct~time.hr,temp.df)
  
  intercept <- coefficients(lm.Bsi) %>% .[1]
  slope <- coefficients(lm.Bsi) %>% .[2]
  
  temp.df$intercept <- intercept
  temp.df$slope <- slope
  
  mean.SiO2.prct <- mean(temp.df$SiO2.prct,na.rm=TRUE)
  
  if (is.na(slope)){
    temp.df$R2 <- NA
    temp.df$P <- NA
    temp.df$extrapolate <- FALSE
    temp.df$SiO2.prct <- mean.SiO2.prct
  } else {
    R2 <- summary(lm.Bsi)$r.squared 
    p <- summary(lm.Bsi)$coefficients[2,4]
    temp.df$R2 <- R2
    temp.df$P <- p
    if(is.na(p)==FALSE & p<=0.05 & R2>=0.65){
      temp.df$extrapolate <- TRUE
      temp.df$SiO2.prct <- intercept
    } else {
      temp.df$extrapolate <- FALSE
      temp.df$SiO2.prct <- mean.SiO2.prct
    }
  }
  
  temp.df <- temp.df %>%
    select(-c(time.hr)) %>%
    unique()
  
  results.list <- c(list(temp.df),results.list)
  
}

#combine results into a dataframe
results.df <- do.call("rbind",results.list)

silica.df <- results.df %>%
  select(c("location","depth.cm","SiO2.mg","SiO2.prct")) %>%
  group_by(location,depth.cm) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  drop_na(location)

save(silica.df,file="Rdata/silica.Rdata")
