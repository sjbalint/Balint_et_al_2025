
rm(list = ls()) #clear environment

library(tidyverse)
library(ggsci)
library(ggforce) #for ellipse

source("scripts/graphing/configure_graphing.R")

data.df <- readRDS("Rdata/compiled_data.rds")

# graphing parameters -----------------------------------------------------

theme <- list(
  basetheme,
  scale_fill_viridis_c(option="rocket",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black"))
)


# graphing ----------------------------------------------------------------

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
  geom_point(data=data.df,aes(x=mean.phi, y=abs(sd.phi),fill=year.mean, color=year.mean, shape=location),
             color="black", size=2, alpha=0.8)+
  scale_x_continuous(trans="log2",limits=c(1,10), breaks=c(1:10),expand = c(0, 0))+
  scale_y_continuous(trans="log2",limits=c(1,5), breaks=c(1:10),expand = c(0, 0))+
  labs(x=bquote("Mean"~"("*phi*")"),
       y=bquote("Sorting"~"("*phi*")"),
       fill="Year",shape="Location")+
  theme(legend.position = "right",
        legend.key.height = unit(0.45, "in"))+
  scale_fill_viridis_c(option="rocket",
                       breaks=c(2000,1900,1800,1700),
                       labels=c("2000","1900","1800","Before\n1800"),
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black"))

ggsave("figures/Fig4.png", width=6, height=5, dpi=600)


