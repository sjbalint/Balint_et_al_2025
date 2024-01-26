#sawyer balint April 2023

rm(list = ls()) #clear environment

# install packages --------------------------------------------------------

#library(devtools)
#devtools::install_github("ropensci/rnaturalearthhires")

library(sf) #for GIS
library(tidyverse)
library(ggspatial)
library(leaflet)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

export_path <- "data/GIS/"


# WHETS boundaries --------------------------------------------------------

xmin=-71.5
xmax=-71.4
ymin=41.5
ymax=41.7

# import natural earth data -----------------------------------------------

ocean.sf <- ne_download(type="ocean",scale="large",category="physical",returnclass = "sf")

save(ocean.sf,file=paste0(export_path,"ocean.Rdata"))


# import NBEP data --------------------------------------------------------

filepath <- "raw/GIS/NBEP/Bays_-_NBEP_2017_(shapefile)/Bays_-_NBEP_2017_(shapefile).shp"

bay.sf <- st_read(dsn = filepath)

save(bay.sf,file=paste0(export_path,"narragansett_bay.Rdata"))

# import ri coastlines ----------------------------------------------------

filepath <- "raw/GIS/RIGIS/Shoreline_and_Islands.gdb"

st_layers(dsn = filepath)

ri_coastlines.sf <- st_read(filepath)

save(ri_coastlines.sf,file=paste0(export_path,"dri_coastlines.Rdata"))

# import ri land use data -------------------------------------------------

filepath <- "raw/GIS/RIGIS/RI_Land_Use.gdb"

st_layers(dsn = filepath)

ri_landuse.sf <- st_read(filepath)

save(ri_landuse.sf,file="data/GIS/ri_landuse.Rdata")


# import census data ------------------------------------------------------

filepath <- "raw/GIS/US Census/cb_2018_us_state_500k/cb_2018_us_state_500k.shp"

st_layers(dsn = filepath)

census_states.sf <- st_read(filepath)

save(census_states.sf,file="data/GIS/census_states.Rdata")


# import road data --------------------------------------------------------

filepath <- "raw/GIS/RIGIS/E-911_Road_Centerlines.gdb"

st_layers(dsn = filepath)

ri_roads.sf <- st_read(filepath) %>%
  st_cast("MULTILINESTRING")

save(ri_roads.sf,file="data/GIS/ri_roads.Rdata")

wickford_roads.sf <- ri_roads.sf %>%
  st_crop(c(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax))

save(wickford_roads.sf,file="data/GIS/wickford_roads.Rdata")
