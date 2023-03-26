
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

legend_title <- NULL

basetheme <- list(
  theme_classic(),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    #panel.grid.major.x = element_line(color="grey"), 
    #panel.grid.major.y = element_line(color="grey"),
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.bottom = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "top")
)

mytheme <- list(
  basetheme,
  geom_hline(data=date_lines.df,aes(yintercept=year.mean,color=location, linetype=type)),
  #geom_label(aes(x=date.depth.cm,y=date.value,label=date.bottom),
              #fill="white",label.size = NA, hjust = 0.8),
  geom_smooth(aes(y=year.mean, x=value, color=location),se=FALSE, orientation="y"),
  #geom_line(aes(x=year.mean,y=value,color=location)),
  geom_errorbar(aes(x=value,ymin=year.min,ymax=year.max,color=location),alpha=0.3),
  #geom_ribbon(aes(xmin=year.min,xmax=year.max,y=value,fill=location), ,alpha=0.1),
  geom_point(aes(y=year.mean, x=value, fill=location, shape=location),
             size=2.5,color="black",alpha=0.7),
  facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "bottom",labeller = label_parsed),
  labs(x=NULL,y="Year",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title),
  scale_color_jco(),
  scale_fill_jco(),
  scale_shape_manual(values=c(21:24)),
  scale_linetype_manual(values=c(3,2,1)),
  scale_y_continuous(breaks=c(2000,1900,1800,1700),
                     labels=c("2000","1900","1800","Colonial\nPeriod"))
)

ylabels.df <- data.frame(name=c('location','depth.cm','%N', "d15N.permil", "%C.total",
                                'd13C.total',"%C.organic",'d13C.organic',"n","P.inorg",
                                "P.total", "P.org","NP","CN","SiO2.prct","SiP",
                                "N.storage","year.mean","clay.pct","sand.pct","gravel.pct",
                                "median.grainsize.phi","accretion.rate.gcm2yr"),
                         factor=as.character(
                           c(
                             bquote("Location"),
                             bquote(atop("Depth","(cm)")),
                             bquote("%"*"N"),
                             bquote(delta^15*N~'(‰)'),
                             bquote("%"*C[Total]),
                             bquote(delta^13*C[Total]~'(‰)'),
                             bquote("%"*C[Organic]),
                             bquote(delta^13*C[Organic]~'(‰)'),
                             bquote("Count"),
                             bquote("%"*P[inorg]),
                             bquote(P[Total]~x~10^-2~'(%)'),
                             bquote("%"*P[organic]),
                             bquote("N:P"~"Ratio"),
                             bquote("C:N"~"Ratio"),
                             bquote("Si"*O[2]~"(%)"),
                             bquote("BSi:P"~"Ratio"),
                             bquote("N"~"Accumulation"~"Rate"),
                             bquote("Year"),
                             bquote("Clay"~"(%)"),
                             bquote("Sand"~"(%)"),
                             bquote("Gravel"~"(%)"),
                             bquote("Median"~"Grainsize"~"("*phi*")"),
                             bquote("Accretion"~"Rate"~"(g/"*cm^2*"/yr)")
                            )
)
)

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


# remove problamatic values -----------------------------------------------

temp.df <- data.df%>%
  filter(depth.cm!=0)

for (row in 1:nrow(temp.df)){
  if (temp.df[row,"NP"]<0 | temp.df[row,"NP"]>40){
    temp.df[row,"NP"] <- NA
  }
  if (temp.df[row,"CN"]>40){
    temp.df[row,"CN"] <- NA
  }
  if (is.na(temp.df[row,"%C.organic"])==FALSE & temp.df[row,"%C.organic"] >10){
    temp.df[row,"%C.organic"] <- NA
  }
  if (is.na(temp.df[row,"%N"])==FALSE & temp.df[row,"%N"]>0.7){
    temp.df[row,"%N"] <- NA
  }
}

data.df <- temp.df

# grainsize ---------------------------------------------------------------


temp.df <- plot_longer(data.df,c("clay.pct","sand.pct","median.grainsize.phi","accretion.rate.gcm2yr"))

ggplot(temp.df)+
  mytheme

ggsave("figures/grainsize.png",width=mywidth, height=myheight)

# elemental composition again ---------------------------------------------

temp.df <- plot_longer(data.df,c("%C.organic","%N","P.total","SiO2.prct"))

ggplot(temp.df)+
  mytheme

ggsave("figures/elements.png",width=mywidth, height=myheight)


# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,c("CN","NP","d15N.permil","d13C.organic"))

line_factors <- temp.df$factor %>%
  unique()

lines.df <- data.frame(factor=line_factors,x=c(NA,16,NA,NA))

ggplot(temp.df)+
  geom_vline(data=lines.df,aes(xintercept=x),linetype="dashed")+
  mytheme

ggsave("figures/element_ratios.png",width=mywidth, height=myheight)


