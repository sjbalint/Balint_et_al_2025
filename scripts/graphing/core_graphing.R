
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

mywidth=10
myheight=6

legend_title <- NULL

#import base graphing theme
load("Rdata/basetheme.Rdata")

mytheme <- list(
  basetheme,
  geom_hline(data=date_lines.df,aes(yintercept=year.mean,color=location, linetype=type)),
  #geom_label(aes(x=date.depth.cm,y=date.value,label=date.bottom),
              #fill="white",label.size = NA, hjust = 0.8),
  geom_smooth(aes(y=year.mean, x=value),se=FALSE, color="black", orientation="y"),
  #geom_line(aes(y=year.mean,x=value,color=location),orientation="y"),
  #geom_errorbar(aes(x=value,ymin=year.min,ymax=year.max,color=location),alpha=0.3),
  #geom_ribbon(aes(xmin=year.min,xmax=year.max,y=value,fill=location), ,alpha=0.1),
  geom_point(aes(y=year.mean, x=value, fill=location, shape=location),
             size=2.5,color="black",alpha=0.7),
  facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "top",labeller = label_parsed),
  labs(x=NULL,y="Year",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title),
  scale_color_jco(),
  scale_fill_jco(),
  scale_shape_manual(values=c(21:24)),
  scale_linetype_manual(values=c(3,2,1)),
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
    pull(factor) %>%
    unique()
  
  plot.df$factor <- factor(plot.df$factor,levels=factor_names,ordered=TRUE)
  
  return (plot.df)
}

# grainsize ---------------------------------------------------------------


temp.df <- plot_longer(data.df,c("sd.phi","mean.phi","accretion.rate.gcm2yr","clay.pct"))

ggplot(temp.df)+
  mytheme

ggsave("figures/grainsize.png",width=mywidth, height=myheight)

# elemental composition again ---------------------------------------------

temp.df <- plot_longer(data.df,c("%C.organic","%N","P.total.pct.e2","SiO2.prct"))

ggplot(temp.df)+
  mytheme

ggsave("figures/elements.png",width=mywidth, height=myheight)


# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,c("C.N.ratio","N.P.ratio","d15N.permil","d13C.organic"))

line_factors <- temp.df$factor %>%
  unique()

lines.df <- data.frame(factor=line_factors,x=c(NA,16,NA,NA))

ggplot(temp.df)+
  geom_vline(data=lines.df,aes(xintercept=x),linetype="dashed")+
  mytheme

ggsave("figures/element_ratios.png",width=mywidth, height=myheight)

# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,c("SiO2.prct","Si.P.ratio","Si.N.ratio"))

line_factors <- temp.df$factor %>%
  unique()

ggplot(temp.df)+
  mytheme

ggsave("figures/Si_ratios.png",width=mywidth, height=myheight)


