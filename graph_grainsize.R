
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)


# import data -------------------------------------------------------------

load("Rdata/grainsize.Rdata")

# graphing parameters -----------------------------------------------------

mywidth=9
myheight=6

legend_title <- NULL

basetheme <- list(
  theme_classic(),
  coord_flip(),
  scale_x_reverse(),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(),
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.bottom = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "right")
)

# grainsize ---------------------------------------------------------------

colourCount = length(unique(grain.df$Class))
getPalette = colorRampPalette(brewer.pal(10, "RdYlBu"))

ggplot(grain.df, aes(x=Depth,y=Percentage, fill=Class))+
  basetheme+
  geom_bar(position="fill",stat="identity",width=2)+
  geom_hline(yintercept=c(0,0.25,0.5,0.75,1))+
  scale_y_continuous(labels=function(y)y*100)+
  facet_wrap(~Location)+
  xlab("Depth\n(cm)")+
  scale_fill_manual(values = getPalette(colourCount))

ggsave("figures/grainsize.png",width=mywidth, height=myheight)