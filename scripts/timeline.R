rm(list = ls()) #clear environment


# install packages --------------------------------------------------------

library(scales)
library(lubridate)
library(tidyverse)
library(knitr)
library(timevis)
library(readxl)
library(ggsci)
library(cowplot)



# load data ---------------------------------------------------------------

#import population data
load("Rdata/population.Rdata")

#import historical timeline data
timeline.df <- read_excel("data/timeline.xlsx") %>%
  arrange(date)

#set type as a factor for graphing
timeline.df$type <- factor(timeline.df$type, 
                           levels=c("Physical Modification","Ecology","Nutrient Cycling"),
                           ordered=TRUE)


# Set the heights we will use for our milestones.
positions <- rep(c(0.5,1,1.5,2,2.5),times=10)

# Set the directions we will use for our milestone (above and below)
directions <- c(1, -1) 


# Assign the positions & directions to each date from those set above.
line_pos.df <- data.frame(
  "date"=unique(timeline.df$date),
  "position"=rep(positions, length.out=length(unique(timeline.df$date))),
  "direction"=rep(directions, length.out=length(unique(timeline.df$date))))

#set position to positive and negative based on direction
line_pos.df$position <- line_pos.df$position * line_pos.df$direction

# Create columns with the specified positions and directions for each milestone event
timeline.df <- left_join(timeline.df, line_pos.df, by="date", all = TRUE) 

#define dates of x axis
date_range <- seq(1800, 2020, by=20)
date_range.df <- data.frame(date_range)


# plot --------------------------------------------------------------------

# offset the labels 0.2 away from scatter points
text_offset <- 0.2 

# Let's use the absolute value since we want to add the text_offset and increase space away from the scatter points 
timeline.df$text_position <- text_offset + abs(timeline.df$position)

# Let's keep the direction above or below for the labels to match the scatter points
timeline.df$text_position <- timeline.df$text_position * timeline.df$direction 

#new variable to match population x axis
timeline.df$Year <- timeline.df$date

#plot of population
p1 <- ggplot(population.df,aes(Year,Population.Thousand))+
  geom_col(color="black",fill="indianred4")+
  labs(y=bquote("Population"~10^3))+
  theme_classic()+
  #remove x axis
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank())

#timeline
p2 <- ggplot(timeline.df,aes(x=Year,y=position, label=description))+
  geom_hline(yintercept=0, color = "black", size=0.7)+
  geom_segment(aes(yend=0,xend=date), color='black', size=0.2)+
  geom_point(aes(shape=type,fill=type),size=3)+
  geom_text(data=date_range.df, aes(x=date_range,y=-0.15, label=date_range),size=3.5, color='black', angle=0)+
  geom_text(aes(y=text_position, color=type), size=3.5, vjust=0.6, show.legend=FALSE)+
  theme_classic()+
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom",)+
  scale_shape_manual(values=21:25)+
  labs(fill=NULL, shape=NULL)+
  scale_fill_jco()+
  scale_color_jco()
 
plot <- plot_grid(p1,p2,ncol=1,align = "v",rel_heights=c(1,2))

ggdraw(plot)

ggsave(plot=plot,"figures/timeline.png",width=8, height=8)
