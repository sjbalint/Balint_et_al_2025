rm(list = ls()) #clear environment

# import packages and data ------------------------------------------------

library(tidyverse)
library(readxl)

# county data -------------------------------------------------------------

county.df <- read.csv("raw/population/nhgis0001_ts_nominal_county.csv") %>%
  filter(STATE=="Rhode Island") %>%
  filter(COUNTY=="Washington County") %>%
  rename("Year"="YEAR",
  "Population"="A00AA") %>%
  select(c("Year","STATE","COUNTY","Population"))

county.df$Region <- "Washington County"


# city data ---------------------------------------------------------------

town.df <- read.csv("raw/population/census_kingstown.csv") %>%
  filter(type=="Summary report") %>%
  pivot_longer(c("north.kingstown.population", "wickford.population"), names_to="Region",values_to="Population")%>%
  mutate(Region=factor(Region,
                         levels=c("north.kingstown.population", "wickford.population"),
                         labels=c("North Kingstown","Wickford"))) %>%
  rename("Year"="year")

# tract data --------------------------------------------------------------

tract.df <- read.csv("raw/population/nhgis0003_ts_nominal_tract.csv") %>%
  filter(STATE=="Rhode Island") %>%
  filter(COUNTY=="Washington County") %>%
  mutate(across(c("STATE","COUNTY","YEAR"),as.factor)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(Population=AT5AA+AT5AB) %>%
  select(c("YEAR","STATE","COUNTY","TRACTA","Population")) %>%
  filter(
    #TRACTA==50102 |
    TRACTA==50302 #|
      #TRACTA==50301|
      #TRACTA==50300
    )

tract.df$YEAR <- factor(tract.df$YEAR,
                        levels=c("1970","1980","1990","2000","2008-2012","2015-2019"),
                        labels=c("1970","1980","1990","2000","2010","2020"))

tract.df$YEAR <- as.numeric(as.character(tract.df$YEAR))

tract.df$Population.Thousand <- tract.df$Population/1000

tract.df <- tract.df %>%
  group_by(YEAR,STATE,COUNTY) %>%
  summarize_all(sum) %>%
  select(-TRACTA) %>%
  rename("Year"="YEAR") %>%
  ungroup()

tract.df$Region <- "Census Tract 503.02"


# combine data ------------------------------------------------------------

population.df <- bind_rows(county.df,town.df, tract.df) %>%
  select(Year, Region, Population) %>%
  mutate(Region=factor(Region, levels=c("Washington County","North Kingstown",
                                        "Wickford","Census Tract 503.02")))

saveRDS(population.df,file="Rdata/population.rds")


