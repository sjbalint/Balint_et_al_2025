
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)


# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

# graphing parameters -----------------------------------------------------

mywidth=9
myheight=6

legend_title <- NULL

basetheme <- list(
  theme_classic(),
  coord_flip(),
  #scale_x_reverse(),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    #panel.grid.major.x = element_line(color="grey"), 
    panel.grid.major.y = element_line(color="grey"),
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.bottom = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "right")
)

mytheme <- list(
  basetheme,
  #geom_vline(aes(xintercept=date.depth.cm,color=location),show_guide = FALSE),
  #geom_label(aes(x=date.depth.cm,y=date.value,label=date.bottom),
              #fill="white",label.size = NA, hjust = 0.8),
  geom_smooth(aes(x=year.mean, y=value, color=location, 
                  #linetype=location
                  ),
              se=FALSE),
  geom_errorbar(aes(y=value,xmin=year.min,xmax=year.max,color=location),alpha=0.5),
  geom_point(aes(x=year.mean, y=value, fill=location, shape=location),
             size=2.5,color="black",alpha=0.7),
  facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "bottom",labeller = label_parsed),
  labs(y=NULL,x="Year",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title),
  scale_color_jco(),
  scale_fill_jco(),
  scale_shape_manual(values=c(21:24)),
  scale_linetype_manual(values=c(3,2,1))
)

ylabels.df <- data.frame(name=c('location','depth.cm','%N', "d15N.permil", "%C.total",
                                'd13C.total',"%C.organic",'d13C.organic',"n","P.inorg",
                                "P.total", "P.org","NP","CN","SiO2.prct","SiP",
                                "N.storage","year.mean"),
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
                             bquote("Year")
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
  
  max_factor <- plot.df$factor %>%
    unique() %>%
    max()
  
  dates.df <- plot.df[,c("location","value","factor","year.mean")] %>%
    filter(factor==max_factor)
  
  dates.df$date.value <- dates.df %>%
    filter(factor==max(factor_names)) %>%
    pull(value) %>%
    max()
  
  #plot.df <- left_join(plot.df,dates.df)
  
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

# plot isotopes -----------------------------------------------------------

temp.df <- plot_longer(data.df,c("d13C.organic","d15N.permil"))

ggplot(temp.df)+
  mytheme

ggsave("figures/isotopes.png",width=mywidth, height=myheight)


# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,c("CN","NP","d15N.permil"))

hline_factors <- temp.df$factor %>%
  unique()

hlines.df <- data.frame(factor=hline_factors,y=c(NA,16,NA))

ggplot(temp.df)+
  geom_hline(data=hlines.df,aes(yintercept=y),linetype="dashed")+
  mytheme

ggsave("figures/element_ratios.png",width=mywidth, height=myheight)



temp.df <- plot_longer(data.df,c("CN","NP"))

hline_factors <- temp.df$factor %>%
  unique()

hlines.df <- data.frame(factor=hline_factors,y=c(NA,16))

ggplot(temp.df)+
  geom_hline(data=hlines.df,aes(yintercept=y),linetype="dashed")+
  mytheme

ggsave("figures/element_ratios_2.png",width=mywidth, height=myheight)


# elemental composition again ---------------------------------------------

temp.df <- plot_longer(data.df,c("%C.organic","%N","P.total"))

ggplot(temp.df)+
  mytheme

ggsave("figures/elements.png",width=mywidth, height=myheight)


# silica ------------------------------------------------------------------

temp.df <- plot_longer(data.df,c("SiO2.prct","SiP","d15N.permil"))

ggplot(temp.df)+
  mytheme

ggsave("figures/silica.png",width=mywidth, height=myheight)

temp.df <- plot_longer(data.df,c("SiO2.prct","SiP"))

ggplot(temp.df)+
  mytheme

ggsave("figures/silica_2.png",width=mywidth, height=myheight)


# N accumulation rates ----------------------------------------------------

temp.df <- plot_longer(data.df,c("CN","N.storage","d15N.permil"))

ggplot(temp.df)+
  mytheme

ggsave("figures/N_storage.png",width=mywidth, height=myheight)

