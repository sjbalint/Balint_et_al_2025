rm(list = ls()) #clear environment

library(tidyverse)
library(readxl)

ysi <- read_excel("data/Week 3.xlsx",sheet="YSI Water Quality")

theme_set(theme_classic())

long_cols <- c("Temperature (C)","Salinity (ppt)")

df.temp <- ysi[c("Date","Location",long_cols)] %>%
  pivot_longer(long_cols)

ggplot(df.temp,aes(x=Date,y=value,color=Location,fill=Location))+
  geom_line()+
  geom_point(shape=21,color="black")+
  facet_wrap(.~name,ncol=1,strip.position = "left")+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.major.y = element_line(size=.1, color="gray")
        )+
  labs(y=NULL)+
  scale_x_datetime(date_labels = "%b %Y",)


# seaweed -----------------------------------------------------------------

seaweed <- read_excel("data/Week 3.xlsx",sheet="Seaweed")

ggplot(seaweed,aes(Color,`Nitrogen (mg)`,fill=Color))+
  geom_boxplot()+
  facet_wrap(.~Location,ncol=1)+
  scale_fill_manual(values=c("brown","forestgreen","red"))

ggplot(seaweed,aes(Location,`Nitrogen (mg)`,fill=Location))+
  geom_boxplot()

# sediment ----------------------------------------------------------------

sediment <- read_excel("data/Week 3.xlsx",sheet="Sediment")

df.temp <- sediment[c("Location","% Nitrogen","% Carbon")] %>%
  pivot_longer(2:3)

ggplot(df.temp,aes(Location,value,fill=Location))+
  geom_boxplot()+
  facet_wrap(.~name,ncol=1,scales="free_y")

# seawater particulates ---------------------------------------------------

seawater <- read_excel("data/Week 3.xlsx",sheet="Seawater Particulates")

ggplot(seawater,aes(Location,`Nitrogen (mg)`,fill=Location))+
  geom_boxplot()

dax

