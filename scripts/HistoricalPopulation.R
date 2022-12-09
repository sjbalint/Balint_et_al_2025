rm(list = ls()) #clear environment

# import packages and data ------------------------------------------------

library(tidyverse)
library(readxl)


# county data -------------------------------------------------------------

county.df <- read.csv("data/nhgis0001_ts_nominal_county.csv") %>%
  filter(STATE=="Rhode Island") %>%
  filter(COUNTY=="Washington County") %>%
  rename("Year"="YEAR",
  "Population"="A00AA") %>%
  select(c("Year","STATE","COUNTY","Population"))

county.df$Population.Thousand <- county.df$Population/1000

county.df$Region <- "County"

theme_set(theme_classic())

ggplot(county.df,aes(Year,Population.Thousand))+
  geom_col(color="black",fill="indianred4")+
  labs(y=bquote("Washington County Population"~10^3))

save(county.df,file="Rdata/county.Rdata")

# tract data --------------------------------------------------------------

tract.df <- read.csv("data/nhgis0003_ts_nominal_tract.csv") %>%
  filter(STATE=="Rhode Island") %>%
  filter(COUNTY=="Washington County") %>%
  mutate(across(c("STATE","COUNTY","YEAR"),as.factor)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(Population=AT5AA+AT5AB) %>%
  select(c("YEAR","STATE","COUNTY","TRACTA","Population")) %>%
  filter(TRACTA==50102 | TRACTA==50302| TRACTA==50301| TRACTA==50300)

tract.df$YEAR <- factor(tract.df$YEAR,
                        levels=c("1970","1980","1990","2000","2008-2012","2015-2019"),
                        labels=c("1970","1980","1990","2000","2010","2020"))

tract.df$YEAR <- as.numeric(as.character(tract.df$YEAR))

tract.df$Population.Thousand <- tract.df$Population/1000

tract.df <- tract.df %>%
  group_by(YEAR,STATE,COUNTY) %>%
  summarize_all(sum) %>%
  select(-TRACTA) %>%
  rename("Year"="YEAR")

tract.df$Region <- "Tract"


# combine data ------------------------------------------------------------

population.df <- rbind(county.df,tract.df)

save(population.df,file="Rdata/population.Rdata")

theme_set(theme_classic())

ggplot(population.df,aes(Year,Population.Thousand,fill=Region))+
  geom_col(color="black",position=position_dodge(width=1))+
  labs(y=bquote("Population"~10^3))+
  scale_fill_grey()




