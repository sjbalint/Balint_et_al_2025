rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)

# import data -------------------------------------------------------------

north.df <- read_excel("raw/dating/US-EPA-WHETS_Pb-Cs_Models_rplum_23.2.8.xlsx", sheet=2) %>%
  mutate(location="North")

middle.df <- read_excel("raw/dating/US-EPA-WHETS_Pb-Cs_Models_rplum_23.2.8.xlsx", sheet=3) %>%
  mutate(location="Middle")

south.df <- read_excel("raw/dating/US-EPA-WHETS_Pb-Cs_Models_rplum_23.2.8.xlsx", sheet=4) %>%
  select(c(1:5)) %>%
  mutate(location="South")

dating.df <- bind_rows(north.df,middle.df,south.df)

colnames(dating.df) <- c("depth.cm","year.min","year.max","year.median","year.mean","location")


# correct for dates -------------------------------------------------------



date_shift <- TRUE

if (date_shift==TRUE){
  for (row in 1:nrow(dating.df)){
    if (dating.df[row,"location"]=="South"){
      dating.df[row,"depth.cm"] <- dating.df[row,"depth.cm"]-10
    }
    if (dating.df[row,"location"]=="North"){
      dating.df[row,"depth.cm"] <- dating.df[row,"depth.cm"]-0
    }
  }
}

save(dating.df,file="Rdata/dating.Rdata")
