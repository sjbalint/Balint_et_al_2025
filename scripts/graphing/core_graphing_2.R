
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

data.df <- data.df %>%
  filter(outlier==FALSE)

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

mywidth=6
myheight=12

legend_title <- NULL

#import base graphing theme
load("Rdata/basetheme.Rdata")

mytheme <- list(
  basetheme,
  geom_vline(data=date_lines.df,aes(xintercept=year.mean,group=location, linetype=type)),
  #geom_label(aes(x=date.depth.cm,y=date.value,label=date.bottom),
  #fill="white",label.size = NA, hjust = 0.8),
  geom_smooth(aes(x=year.mean, y=value),se=FALSE, color="black", orientation="x"),
  #geom_line(aes(y=year.mean,x=value,color=location),orientation="y"),
  #geom_errorbar(aes(x=value,ymin=year.min,ymax=year.max,color=location),alpha=0.3),
  #geom_ribbon(aes(xmin=year.min,xmax=year.max,y=value,fill=location), ,alpha=0.1),
  geom_point(aes(x=year.mean, y=value, fill=cluster, shape=cluster),
             size=2.5,color="black",alpha=0.7),
  facet_wrap(~factor,ncol=1,scales="free_y",strip.position = "left",labeller = label_parsed),
  labs(y=NULL,x="Year",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title),
  scale_color_jco(),
  scale_fill_jco(),
  scale_shape_manual(values=c(21:24)),
  scale_linetype_manual(values=c(3,2,1)),
  scale_x_continuous(breaks=c(2000,1900,1800,1700),
                     labels=c("2000","1900","1800","Before\n1800")),
  theme(strip.text.y.left = element_text(angle=90))
)

#import ylabels.df for the plot_longer function
load("Rdata/graphing_labels.Rdata")

plot_longer <- function(data.df,long_cols){
  plot.df <- data.df %>%
    pivot_longer(long_cols)
  
  plot.df <- left_join(plot.df,ylabels.df)
  
  factor_names <- plot.df %>%
    pull(factor2) %>%
    unique()
  
  plot.df$factor <- factor(plot.df$factor2,levels=factor_names,ordered=TRUE)
  
  return (plot.df)
}

# grainsize ---------------------------------------------------------------


temp.df <- plot_longer(data.df,c("mean.phi","sd.phi","accretion.rate.gcm2yr",
                                 "%C.organic","%N","P.total.pct.e2"))

ggplot(temp.df)+
  mytheme

ggsave("figures/bigplot_1.png",width=mywidth, height=myheight)