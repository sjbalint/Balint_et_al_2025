
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)


# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

date_lines.df <- data.df %>%
  select(location,year.mean,end.pb,cs.peak) %>%
  filter(!is.na(end.pb) | !is.na(cs.peak))

for (row in 1:nrow(date_lines.df)){
  if (!is.na(date_lines.df[row,"end.pb"])){
    date_lines.df[row,"type"] <- "Pb-210 Minimum"
  }
  if (!is.na(date_lines.df[row,"cs.peak"])){
    date_lines.df[row,"type"] <- "Cs-237 Maximum"
  }
}

# graphing parameters -----------------------------------------------------

mywidth=10
myheight=6

#import base graphing theme
load("Rdata/basetheme.Rdata")

mytheme <- list(
  basetheme,
  geom_point(aes(y=SiO2.prct, x=value, fill=year.median, shape=location),
             size=2.5,color="black",alpha=0.7),
  facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "bottom",labeller = label_parsed),
  labs(x=NULL,y=bquote(atop("Si"*O[2],"(%)")),shape=NULL,fill=NULL),
  scale_fill_viridis_c(option="cividis",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black")),
  scale_shape_manual(values=c(21:24)),
  scale_x_continuous(position = "bottom"),
  theme(legend.key.width = unit(0.5, "in"))
)

#import ylabels.df for the plot_longer function
load("Rdata/graphing_labels.Rdata")

plot_longer <- function(data.df,value_cols){
  plot.df <- data.df %>%
    select(all_of(c("location","year.median","SiO2.prct",value_cols))) %>%
    pivot_longer(value_cols)
  
  plot.df <- left_join(plot.df,ylabels.df)
  
  factor_names <- plot.df %>%
    pull(factor) %>%
    unique()
  
  plot.df$factor <- factor(plot.df$factor,levels=factor_names,ordered=TRUE)
  
  return (plot.df)
}


# remove problamatic values -----------------------------------------------

temp.df <- data.df%>%
  filter(depth.cm!=0)

for (row in 1:nrow(temp.df)){
  if (temp.df[row,"N.P.ratio"]<0 | temp.df[row,"N.P.ratio"]>40){
    temp.df[row,"N.P.ratio"] <- NA
  }
  if (temp.df[row,"C.N.ratio"]>40){
    temp.df[row,"C.N.ratio"] <- NA
  }
  if (is.na(temp.df[row,"%C.organic"])==FALSE & temp.df[row,"%C.organic"] >10){
    temp.df[row,"%C.organic"] <- NA
  }
  if (is.na(temp.df[row,"%N"])==FALSE & temp.df[row,"%N"]>0.7){
    temp.df[row,"%N"] <- NA
  }
}

data.df <- temp.df


# graph of d15N -----------------------------------------------------------

plot.df <- plot_longer(data.df,c("d15N.permil","%N","N.P.ratio"))

ggplot(plot.df)+
  mytheme

ggsave("figures/Si/nitrogen.png",width=mywidth, height=myheight)



plot.df <- plot_longer(data.df,c("d13C.organic","%C.organic","C.N.ratio"))

ggplot(plot.df)+
  mytheme

ggsave("figures/Si/carbon.png",width=mywidth, height=myheight)
