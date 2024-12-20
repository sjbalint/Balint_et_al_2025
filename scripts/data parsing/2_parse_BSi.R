rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)

# import data -------------------------------------------------------------

filepath <- "raw/silica/"

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

colnames(seal.df) <- c("Sample.ID","peak","type","SiO2.uM","ad.values") #set column names

#the seal records "NA" values as 0 which causes problems
seal.df["SiO2.uM"][seal.df["SiO2.uM"] == "0"] <- NA

# instrument performance --------------------------------------------------

#determine number of QC sampels
qc.n <- seal.df[grep("%", seal.df$Sample.ID),] %>% #the qcs all have "%" in the same
  count()

#calculate standard deviation of QC samples
qc.sd <- seal.df[grep("%", seal.df$Sample.ID),] %>% #the qcs all have "%" in the same
  summarize_all(sd,na.rm=TRUE) %>%
  pull(SiO2.uM)

print(qc.sd) #print the standard deviation

#determine the number of MDL samples run
mdl.n <- seal.df[grep("MDL", seal.df$Sample.ID),] %>% #the MDL samples have "MDL" in the name
  filter(between(SiO2.uM,0,40)) %>%
  count()

#calculate standard deviation of MDL
mdl.sd <- seal.df[grep("MDL", seal.df$Sample.ID),] %>%
  filter(between(SiO2.uM,0,40)) %>%
  summarize_all(sd,na.rm=TRUE) %>%
  pull(SiO2.uM) %>%
  as.numeric()

#calculate MDL
df <- as.numeric(mdl.n-1)
qt <- qt(0.99,df) #students t test value
mdl <- qt*mdl.sd #mdl = students t test value times the standard deviation of the mdls

print(mdl)


# calculate average -------------------------------------------------------

#all samples were analyzed with an instrument duplicate, so we want the average

seal.df <- seal.df %>%
  mutate(across(c("type"),as.factor)) %>% #tidy the data
  mutate(across(c("peak","SiO2.uM","ad.values"),as.numeric)) %>% #ditto
  group_by(Sample.ID) %>%
  summarize_all(mean,na.rm=TRUE) %>% #calculate mean by sample ID.
  ungroup()

str(seal.df) #print the first 6 lines of our dataframe


# parse sample ID ---------------------------------------------------------

seal.df$id <- str_sub(seal.df$Sample.ID,0,-2) %>% #return numeric sample identifier
  as.numeric()

seal.df$time <- str_sub(seal.df$Sample.ID,start=-1) #return letter time identifier

#convert letter time identifier to number of hours.
#i.e. the samples labeled "A" were extracted after 3 hours of digestion
seal.df$time.hr <- factor(seal.df$time,levels=c("A","B","C"), labels=c(3,4,5)) %>%
  as.character() %>%
  as.numeric()

silica.df <- left_join(seal.df,weights.df) #add the weights to the dataframe


# calculate percent SiO ---------------------------------------------------

silica.df$SiO2.mg <- silica.df$SiO2.uM * 60.08 * 0.04 * 10 * 0.1 #from the R script shared by Mel

silica.df$SiO2.prct <- silica.df$SiO2.mg/silica.df$final.mg #caluclate percent

silica.df <- silica.df %>%
  select(c("id","time.hr","replicate","location","depth.cm","SiO2.mg","SiO2.prct"))


# linear regression -------------------------------------------------------

#now we want to perform a linear regression between the three time points

id.list <- silica.df %>% #create list of sample ids. each id has three measurements associated with it (A,B,C)
  drop_na(c("id","depth.cm","SiO2.prct")) %>%
  pull(id) %>%
  unique()

results.list <- list() #create a list to store the data

for (myid in id.list){
  temp.df <- silica.df %>% #temporary dataframe
    filter(id==myid)
  
  ###### the commented out code below was in the R script shared by Mel, but it was already commented
  
  #recode time variable to always start with 0 so intercept is calculated back to first time point
  #mintime <- min(temp.df$time.hr)
  #temp.df <- temp.df %>% 
   # mutate(time.hr=time.hr-mintime) %>%
    #data.frame()
  
  #make linear model
  lm.Bsi <- lm(SiO2.prct~time.hr,temp.df) #linear model of percent and time
  
  intercept <- coefficients(lm.Bsi) %>% .[1]#return intercept
  slope <- coefficients(lm.Bsi) %>% .[2] #return slope
  
  #add slope and intercept to temporary dataframe
  temp.df$intercept <- intercept
  temp.df$slope <- slope
  
  #calculate the mean across all three measurements (A,B,C)
  mean.SiO2.prct <- mean(temp.df$SiO2.prct,na.rm=TRUE)
  
  #now we need to decide if we are going to use the results from the linear regression or not.
  #if the sample is entirely composed of biogenic silica (no mineral), then the linear regression may not be accurate
  #and we would us the mean instead
  if (is.na(slope)){ #if the linear regression did not return a slope, just use the mean
    temp.df$R2 <- NA
    temp.df$P <- NA
    temp.df$extrapolate <- FALSE
    temp.df$SiO2.prct <- mean.SiO2.prct
  } else { #otherwise calculate R2 and p value
    R2 <- summary(lm.Bsi)$r.squared 
    p <- summary(lm.Bsi)$coefficients[2,4]
    temp.df$R2 <- R2
    temp.df$P <- p
    #only use the linear regression of the p value is <= 0.05 and the r2 value is >= 0.65
    #adopted from Mel's R script
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
  
  results.list <- c(list(temp.df),results.list) #add the temporary dataframe to the results list
  
}

#combine results into a single dataframe
results.df <- do.call("rbind",results.list)

#i weighed samples in duplicate, so now calculate the mean of those
silica.df <- results.df %>%
  select(c("location","depth.cm","SiO2.mg","SiO2.prct")) %>%
  group_by(location,depth.cm) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  drop_na(location)

#save file
saveRDS(silica.df,file="Rdata/silica.rds")
