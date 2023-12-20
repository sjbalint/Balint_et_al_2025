
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
  #filter(cluster!=4) %>%
  drop_na(cluster)

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

legend_title <- NULL

#import base graphing theme
load("Rdata/basetheme.Rdata")

mytheme <- list(
  basetheme,
  geom_hline(data=date_lines.df,aes(yintercept=year.mean,group=location, linetype=type)),
  #geom_label(aes(x=date.depth.cm,y=date.value,label=date.bottom),
              #fill="white",label.size = NA, hjust = 0.8),
  geom_smooth(aes(y=year.mean, x=value),se=FALSE, color="black", orientation="y"),
  #geom_line(aes(y=year.mean,x=value,color=location),orientation="y"),
  #geom_errorbar(aes(x=value,ymin=year.min,ymax=year.max,color=location),alpha=0.3),
  #geom_ribbon(aes(xmin=year.min,xmax=year.max,y=value,fill=location), ,alpha=0.1),
  geom_point(aes(y=year.mean, x=value, fill=cluster),
             shape=21, size=2,color="black"),
  geom_point(aes(y=year.mean, x=value, shape=location),
             size=2,color="black", fill="grey"),
  geom_point(aes(y=year.mean, x=value, fill=cluster, shape=location),
             size=2.5,color="black",show.legend=FALSE),
  facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "top",labeller = label_parsed),
  labs(x=NULL,y="Year",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title),
  scale_y_continuous(breaks=c(2000,1900,1800,1700),
                     labels=c("2000","1900","1800","Before\n1800"))
)

#import ylabels.df for the plot_longer function
load("Rdata/graphing_labels.Rdata")

plot_longer <- function(data.df,long_cols){
  plot.df <- data.df %>%
    pivot_longer(long_cols)
  
  plot.df <- left_join(plot.df,ylabels.df)
  
  factor_names <- plot.df %>%
    pull(factor1) %>%
    unique()
  
  plot.df$factor <- factor(plot.df$factor1,levels=factor_names,ordered=TRUE)
  
  return (plot.df)
}


# HCA results -------------------------------------------------------------

plot.df <- data.df %>%
  select(location, year.mean, cluster) %>%
  group_by(location, cluster) %>%
  mutate(year.max=max(year.mean),
         year.min=min(year.mean)) %>%
  ungroup() %>%
  select(-year.mean) %>%
  unique()

write.csv(plot.df, "output/HCA_clusters.csv", row.names=FALSE)

ggplot(plot.df, aes(x=location, xend=location, y=year.min, yend=year.max, color=cluster))+
  basetheme+
  scale_x_discrete(position = "top")+
  geom_segment(linewidth=2)+
  geom_segment(linewidth=15, show.legend=FALSE)+
  #scale_color_viridis_d(option="cividis", direction = -1, aesthetics = c("colour", "fill"))+
  labs(x=NULL, y="Year", color="Cluster")

ggsave("figures/HCA.png",width=6, height=4)

# grainsize ---------------------------------------------------------------


temp.df <- plot_longer(data.df,c("sand.pct","mean.phi","accretion.rate.gcm2yr","sd.phi"))

ggplot(temp.df)+
  mytheme

ggsave("figures/grainsize.png",width=mywidth, height=myheight)

# elemental composition again ---------------------------------------------

temp.df <- plot_longer(data.df,c("%C.organic","%N","P.total.pct.e2"))

ggplot(temp.df)+
  mytheme

ggsave("figures/elements.png",width=mywidth, height=myheight)


# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,c("d15N.permil","SiO2.prct","d13C.organic"))

line_factors <- temp.df$factor %>%
  unique()

ggplot(temp.df)+
  mytheme

ggsave("figures/isotopes.png",width=mywidth, height=myheight)

# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,c("SiO2.prct","Si.P.ratio","Si.N.ratio"))

line_factors <- temp.df$factor %>%
  unique()

ggplot(temp.df)+
  mytheme

ggsave("figures/Si_ratios.png",width=mywidth, height=myheight)


# additional ratios -------------------------------------------------------

temp.df <- plot_longer(data.df,c("C.N.ratio","C.P.ratio", "N.P.ratio"))

line_factors <- temp.df$factor %>%
  unique()

lines.df <- data.frame(factor=line_factors,x=c(6.625,106,16))

ggplot(temp.df)+
  geom_vline(data=lines.df,aes(xintercept=x),linetype="dashed")+
  mytheme

ggsave("figures/element_ratios.png",width=mywidth, height=myheight)


# chronology --------------------------------------------------------------

temp.df <- plot_longer(data.df,c("C.N.ratio","C.P.ratio", "N.P.ratio"))

line_factors <- temp.df$factor %>%
  unique()

lines.df <- data.frame(factor=line_factors,x=c(6.625,106,16))

ggplot(temp.df)+
  geom_vline(data=lines.df,aes(xintercept=x),linetype="dashed")+
  mytheme

ggsave("figures/element_ratios.png",width=mywidth, height=myheight)
