rm(list = ls()) #clear environment

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)
library(G2Sd)
library(cowplot)
library(ggforce)


# example -----------------------------------------------------------------

#data.df <- granulo

grain_stats <- function(data.df){
  
  manual.df <- data.df %>%
    mutate(phi = log2(as.numeric(rownames(data.df)))) %>%
    pivot_longer(-phi) %>%
    mutate(mean.phi=phi*value) %>%
    group_by(name) %>%
    mutate(mean.phi = sum(mean.phi)/100) %>%
    ungroup() %>%
    mutate(sd.phi=value*(phi-mean.phi)^2) %>%
    group_by(name) %>%
    mutate(sd.phi=sqrt(sum(sd.phi)/100)) %>%
    ungroup() %>%
    select(-c("value","phi")) %>%
    unique()
  
  summary.df <- granstat(data.df)
  
  myrows <- c("Kurtosis.fw.um","Skewness.fw.um") #sorting = standard deviation
  
  log.df <- summary.df[rownames(summary.df) %in% myrows, ] %>%
    t() %>%
    data.frame() %>%
    mutate_all(as.numeric)
  
  colnames(log.df) <- c("skewness.um","kurtosis.um")
  
  log.df$mean.phi <- manual.df$mean.phi
  log.df$sd.phi <- manual.df$sd.phi
  
  log.df$name <- rownames(log.df)
  
  return(log.df)
}


# do it for real ----------------------------------------------------------

load("Rdata/dating.Rdata")

all_cores.df <- read_excel("raw/grainsize/grainsize_tidy.xlsx") %>%
  select(-c(Replicate,Pseudoreplicate,Core)) %>%
  group_by(Location,Depth) %>%
  summarize_all(mean, na.rm=TRUE) %>%
  ungroup()

widen_cores <- function(data.df,location){
  temp.df <- data.df %>%
    filter(Location==location) %>%
    select(-Location)
  
  depths <- temp.df %>%
    pull(Depth)
  
  temp.df <- temp.df %>%
    select(-Depth) %>%
    t() %>%
    data.frame()
  
  colnames(temp.df) <- depths
  
  rownames(temp.df) <- as.numeric(rownames(temp.df))
  
  return(temp.df)
}

# perform statistics ------------------------------------------------------

north.df <- widen_cores(all_cores.df,"North") %>%
  grain_stats() %>%
  mutate(location="North")

middle.df <- widen_cores(all_cores.df,"Middle")%>%
  grain_stats() %>%
  mutate(location="Middle")

south.df <- widen_cores(all_cores.df,"South")%>%
  grain_stats()%>%
  mutate(location="South")

grain.df <- bind_rows(north.df,middle.df,south.df) %>%
  mutate(depth.cm=as.numeric(substr(name,2,20))) %>%
  select(-name)

grain.df <- left_join(grain.df,dating.df) %>%
  drop_na(century)

temp.df <- widen_cores(all_cores.df,"North")

temp.df <- granstat(temp.df)

# graphing parameters -----------------------------------------------------

basetheme <- list(
  theme_classic(),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    #strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.bottom = element_text(size=12),
    #axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "right",
    #legend.title=element_blank()
    ),
  scale_shape_manual(values=c(21:25)),
  scale_fill_viridis_c(option="cividis",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black"))
)


# graphing ----------------------------------------------------------------

lines.df <- data.frame(x1=c(1,2.5),
                       y1=c(1.5,1),
                       x2=c(2.5,10),
                       y2=c(5,5))

polygon.df <- data.frame(x1=c(1,2.5,10,2.5,1),
                         y1=c(1.5,5,5,1,1))

curves.df <- data.frame(x1=c(1.5),
                        y1=c(5),
                        x2=c(7),
                        y2=c(1))

text.df <- data.frame(x=c(2.5, 7),
                      y=c(2.5, 1.25),
                      text=c(
                             "Fluvial and\nstorm episodes",
                             "Closed\nBasin"))

ggplot()+
  basetheme+
  geom_curve(data=curves.df,aes(x=x1,y=y1,xend=x2,yend=y2), color="black",curvature=0.1)+
  geom_polygon(data=polygon.df,aes(x=x1,y=y1),color=NA,fill="grey90")+
  geom_segment(data=lines.df,aes(x=x1,y=y1,xend=x2,yend=y2), color="black")+
  #geom_ellipse(aes(x0 = 5.5, y0 = 3, a = 0.3, b = 0.8, angle = 45), fill="white")+
  geom_text(data=text.df,aes(x=x,y=y,label=text))+
  geom_point(data=grain.df,aes(x=mean.phi, y=abs(sd.phi),fill=year.mean, color=year.mean, shape=location),
             color="black", size=2, alpha=0.8)+
  scale_x_continuous(trans="log2",limits=c(1,10), breaks=c(1:10),expand = c(0, 0))+
  scale_y_continuous(trans="log2",limits=c(1,5), breaks=c(1:10),expand = c(0, 0))+
  labs(x=bquote("Mean"~"("*phi*")"),
       y=bquote("Sorting"~"("*phi*")"),
       fill="Year",shape="Location")

ggsave("figures/grain_sorting.png", width=6, height=5)

