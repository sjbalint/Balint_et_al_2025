rm(list = ls()) #clear environment

# install packages --------------------------------------------------------

#library(devtools)
#devtools::install_github("ropensci/rnaturalearthhires")

library(sf) #for GIS
library(tidyverse) #for data manipulation
library(ggsci) #for color palette
library(cowplot) #for graphic design
library(ggpmisc) #for automatic positioning of inset
library(ggspatial)
library(readxl) #to import data


# import data -------------------------------------------------------------

#import WHETS stations
whets.df <- read_excel("raw/sampling_locations.xlsx",sheet="WHETS")

#convert to sf object
whets.sf <- st_as_sf(whets.df,coords=c("Longitude","Latitude"),crs="+proj=longlat +datum=WGS84")

#import GIS datasets
load("Rdata/GIS/ocean.Rdata")
load("Rdata/GIS/narragansett_bay.Rdata")
load("Rdata/GIS/census_states.Rdata")
load("Rdata/GIS/wickford_roads.Rdata")

# configure_graphing ------------------------------------------------------

#colors we want
land_color="antiquewhite"
water_color="aliceblue"

load("Rdata/basetheme.Rdata")

mytheme <- list(
  basetheme,
  theme_bw(),
  scale_fill_jco(),
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(fill="white",colour = "black"),
        panel.grid=element_blank())
)

#theme for the map inset. this just removes the axis ticks
insettheme <- list(
  theme(plot.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
)

# make a map of wickford --------------------------------------------------

#the location boundaries for the wickford map
xmin=-71.465
xmax=-71.41
ymin=41.56
ymax=41.59

roads.sf <- wickford_roads.sf %>%
  mutate(alpha=factor(St_Class,levels=c(3,9,30,40),labels=c(0.3,0.3,0.7,0.7)),
         alpha=as.numeric(as.character(alpha)))

p1 <- ggplot() +
  mytheme+
  geom_sf(data=bay.sf,fill="aliceblue")+
  geom_sf(data=roads.sf, aes(alpha=alpha), color="black",
          show.legend=FALSE
          )+
  geom_sf(data=whets.sf,shape=21,fill="firebrick",size=5)+
  geom_sf_text(data=whets.sf,aes(label=Station),size=5,nudge_x = 0.001,nudge_y = 0.001)+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)+
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(panel.background = element_rect(fill = "antiquewhite"),
        legend.position=c(0.85,0.85))+
  scale_alpha_identity()

#p1

# make a map of narragansett bay ------------------------------------------

p2 <- ggplot() +
  mytheme+
  geom_sf(data=ocean.sf,fill=water_color)+
  geom_sf(data=census_states.sf, fill=land_color)+
  geom_sf(data=bay.sf,fill=water_color)+
  coord_sf(xlim = c(-71.5, -71.1), ylim = c(41.9, 41.4), expand = FALSE)+
  geom_rect(aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill = NA, color = "black",linewidth = 1)

inset <- tibble(x = 10, 
                y = 0.01, 
                plot = list(p2+insettheme))


# make an inset map of wickford -------------------------------------------

xmin=-71.452
xmax=-71.457
ymin=41.568
ymax=41.572

p3 <- ggplot() +
  mytheme+
  geom_sf(data=bay.sf,fill="aliceblue")+
  geom_sf(data=roads.sf, aes(alpha=alpha), color="black",
          show.legend=FALSE
  )+
  geom_sf(data=whets.sf,shape=21,fill="firebrick",size=5)+
  geom_sf_text(data=whets.sf,aes(label=Station),size=5,nudge_x = 0.0005,nudge_y = 0.0001)+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)+
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(panel.background = element_rect(fill = "antiquewhite"),
        legend.position=c(0.85,0.85))+
  scale_alpha_identity()

inset2 <- tibble(x = 10, 
                y = 0.01, 
                plot = list(p2+insettheme))

# combine plots -----------------------------------------------------------

final <- p1+
  geom_plot_npc(data = inset, aes(npcx = x, npcy = y, label = plot, vp.width = 0.3, vp.height =0.6))

#to save time, the plot does not render by default.
#final

ggsave(plot=final,"figures/GIS/WHETS.png")

