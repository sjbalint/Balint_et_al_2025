
rm(list = ls()) #clear environment

# install packages --------------------------------------------------------

library(ggsci)
library(ggforce) #for ellipse
library(plyr) #for round_any
library(tidyverse)
library(viridis) #to define colors manually

# import data -------------------------------------------------------------

source("scripts/graphing/configure_graphing.R")

aspect_ratio=5/6

mywidth=81*2

myheight=mywidth*aspect_ratio

data.df <- readRDS("Rdata/compiled_data.rds") %>%
  mutate(century=as.character(round_any(year.mean, 100, f=floor)),
         century=ifelse(depth.cm<15 & is.na(century), "2000", century),
         century=ifelse(depth.cm>40 & is.na(century), "1700", century),)

class.df <- readRDS("Rdata/grainclass.rds")

# bivariate plot ----------------------------------------------------------


ellipse.df <- data.frame(x0=c(6, 4.5, 2.7),
                         y0=c(3.1, 2.4, 2.3),
                         a=c(0.2, 0.3, 0.4),
                         b=c(0.4, 0.45, 0.8),
                         angle=c(40, -40, -40))

arrow.df <- data.frame(x1=c(6.2, 5, 1.7),
                       y1=c(4.1, 1.65, 3.1),
                       x2=c(6, 4.5, 2.2),
                       y2=c(3.5, 2, 2.3))

text.df <- data.frame(x=c(1.6, 5, 6),
                      y=c(3.5, 1.5, 4.5),
                      text=c("Fluvial and\nstorm episodes",
                             "Closed\nBasin",
                             "Partially open estuary\nto restricted estuary"))

ggplot()+
  basetheme+
  geom_ellipse(data=ellipse.df, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle*pi/180))+
  geom_segment(data=arrow.df,aes(x=x1,y=y1,xend=x2,yend=y2), color="black",
               arrow=arrow(length = unit(0.1,"cm")))+
  geom_text(data=text.df,aes(x=x,y=y,label=text))+
  geom_point(data=data.df,aes(x=mean.phi, y=abs(sd.phi),fill=century, color=century, shape=location),
             color="black", size=2, alpha=0.8)+
  scale_x_continuous(trans="log2",limits=c(1,10), breaks=c(1:10),expand = c(0, 0))+
  scale_y_continuous(trans="log2",limits=c(1,5), breaks=c(1:10),expand = c(0, 0))+
  labs(x=bquote("Mean"~"("*phi*")"),
       y=bquote("Sorting"~"("*phi*")"),
       fill="Century",shape="Location")+
  theme(legend.position = "right",
        legend.key.height = unit(0.45, "in"))+
  scale_fill_viridis_d(option="rocket",
                       breaks=c(2000,1900,1800,1700),
                       labels=c("2000","1900","1800","Before\n1800"))

ggsave("figures/Fig4.pdf", width=mywidth, height=myheight, units="mm")

# size class plot ---------------------------------------------------------

colors1 <- viridis(6, option = "rocket")[1:6]
colors2 <- viridis(7, option = "mako")[6:1]

ggplot(class.df, aes(x=depth.cm+thickness.cm/2,y=percentage, fill=class, width=thickness.cm))+
  basetheme+
  geom_bar(position="fill",stat="identity", color="black", linewidth=0.2)+
  coord_flip()+
  #scale_x_reverse(labels=function(x)2020-(x*3))+
  scale_x_reverse(expand=expansion(c(0,0)))+
  scale_y_continuous(labels=function(y)y*100)+
  facet_wrap(~location)+
  theme(legend.position="right")+
  labs(x="Depth (cm)",
       y="Percentage",
       fill="Classification")+
  scale_fill_manual(values = c(colors1, colors2))

ggsave("figures/FigS4.pdf",width=mywidth, height=myheight, units="mm")
