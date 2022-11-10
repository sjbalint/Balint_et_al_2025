rm(list = ls()) #clear environment

# import packages and data ------------------------------------------------

library(tidyverse)
library(readxl)
library(ggpubr)
library(ggpmisc)
library(progress)
library(cowplot)
library(lubridate)
library(viridis)
library(ggthemes)

pop <- read.csv("data/nhgis0001_ts_nominal_county.csv") %>%
  filter(STATE=="Rhode Island") %>%
  filter(COUNTY=="Washington County") %>%
  rename("Year"="YEAR",
  "Population"="A00AA")

pop$Population.Thousand <- pop$Population/1000

theme_set(theme_classic())

ggplot(pop,aes(Year,Population.Thousand))+
  geom_col(color="black",fill="indianred4")+
  labs(y=bquote("Population"~10^3))


