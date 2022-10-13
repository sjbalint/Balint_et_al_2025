
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)


# import data -------------------------------------------------------------

load("Rdata/iso.Rdata")

# graphing parameters -----------------------------------------------------

theme_set(theme_classic())

mywidth=11
myheight=8.5

legend_title <- NULL

mytheme <- list(
  geom_smooth(aes(x=depth.cm, y=value, color=location, 
                  #linetype=location
                  ),
              se=FALSE),
  geom_point(aes(x=depth.cm, y=value, fill=location, shape=location),
             size=2.5,color="black",alpha=0.7),
  coord_flip(),
  scale_x_reverse(),
  facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "bottom",labeller = label_parsed),
  labs(y=NULL,x="Depth\n(cm)",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(size=.1, color="gray"),
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.bottom = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "right"),
  scale_color_viridis_d(),
  scale_fill_viridis_d(),
  scale_shape_manual(values=c(21:24)),
  scale_linetype_manual(values=c(3,2,1))
)

ylabels.df <- data.frame(name=c('location','depth.cm','%N', "d15N.permil", "%C.total",
                                'd13C.total',"%C.organic",'d13C.organic',"n","P.inorg",
                                "P.total", "P.org","NP","CN"),
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
                             bquote("C:N"~"Ratio")
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
  
  plot.df$factor <- factor(plot.df$factor,levels=factor_names)
  
  return (plot.df)
}


# remove problamatic values -----------------------------------------------

temp.df <- iso.df

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

iso.df <- temp.df

# plot isotopes -----------------------------------------------------------

temp.df <- plot_longer(iso.df,c("d13C.organic","d15N.permil"))

ggplot(temp.df)+
  mytheme

ggsave("figures/isotopes.png",width=mywidth, height=myheight)


# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(iso.df,c("CN","NP","d15N.permil"))

ggplot(temp.df)+
  mytheme

ggsave("figures/element_ratios.png",width=mywidth, height=myheight)


# elemental composition again ---------------------------------------------

temp.df <- plot_longer(iso.df,c("%C.organic","%N","P.total"))

ggplot(temp.df)+
  mytheme

ggsave("figures/elements.png",width=mywidth, height=myheight)


# grainsize ---------------------------------------------------------------

grain.df <- read_excel("data/grainsize_tidy.xlsx") %>%
  select(-c(Replicate,Pseudoreplicate,Core)) %>%
  group_by(Location,Depth) %>%
  summarize_all(mean) %>%
  pivot_longer(!c(Location,Depth),names_to="Micrometers",values_to="Percentage")

grain.df$Location <- factor(grain.df$Location,levels=c("North","Middle","South"))

grain.df$Micrometers <- as.numeric(grain.df$Micrometers)

mysizes.df <- data.frame(as.numeric(unique(grain.df$Micrometers)))
colnames(mysizes.df) <- c("Micrometers")

size_classes <- c("Clay","V.F. Silt","F. Silt","M. Silt","C. Silt","V.C. Silt",
                  "V.F. Sand","F. Sand","Sand","C. Sand","V.C. Sand","V.F. Gravel")

grainsizes.df <- data.frame(size_classes,c(2,4,8,16,31,62,125,250,500,1000,2000,4000))
colnames(grainsizes.df) <- c("Class","Micrometers")

mypalette <- brewer.pal(n = length(size_classes), name = "RdYlBu")

for (row in 1:nrow(mysizes.df)){
  for (row2 in 1:nrow(grainsizes.df)){
    if (mysizes.df[row,"Micrometers"]<grainsizes.df[row2,"Micrometers"]){
      mysizes.df[row,"Class"] <- grainsizes.df[row2,"Class"]
      break
    }
  }
}

grain.df <- left_join(grain.df,mysizes.df)

grain.df$Class <- factor(grain.df$Class,levels=size_classes)

ggplot(grain.df, aes(x=Depth,y=Percentage, fill=Class))+
  geom_bar(position="fill",stat="identity",width=2)+
  geom_hline(yintercept=0)+
  geom_hline(yintercept=.25)+
  geom_hline(yintercept=.5)+
  geom_hline(yintercept=.75)+
  geom_hline(yintercept=1)+
  coord_flip()+
  #scale_x_reverse(labels=function(x)2020-(x*3))+
  scale_x_reverse()+
  scale_y_continuous(labels=function(y)y*100)+
  facet_wrap(~Location)+
  xlab("Depth\n(cm)")+
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(),
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.top = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "right")+
  scale_fill_brewer(palette="RdYlBu")

ggsave("figures/grainsize.png",width=mywidth, height=myheight)
