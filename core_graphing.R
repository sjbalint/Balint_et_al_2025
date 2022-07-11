
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

require(tidyverse)
require(readxl)
require(progress)


# import data -------------------------------------------------------------

iso.df <- read_excel("data/isotopes_07082022.xlsx",sheet="isotopes")

P.df <- read_excel("data/isotopes_07082022.xlsx",sheet="phosphorus")

iso.df <- left_join(iso.df,P.df)

iso.df$NP <- iso.df$N/iso.df$P.total

iso.df$P.total <- iso.df$P.total*100

iso.df$location <- factor(iso.df$location,levels=c("North","Middle","South"))


# graphing parameters -----------------------------------------------------

theme_set(theme_classic())

mywidth=11
myheight=8.5

legend_title <- NULL

mytheme <- list(
  geom_smooth(se=FALSE,alpha=0.3),
  geom_point(size=2,color="black"),
  coord_flip(),
  scale_x_reverse(),
  facet_wrap(~name,nrow=1,scales="free_x",strip.position = "bottom",labeller = label_parsed),
  labs(y=NULL,x="Depth\n(cm)",shape=legend_title,color=legend_title,fill=legend_title),
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
    scale_shape_manual(values=c(21:24))
)


# plot isotopes -----------------------------------------------------------

long_cols <- c("d13C.org","d15N")
label_list <- c(bquote(delta^13*C~'(‰)'),
                bquote(delta^15*N~'(‰)'))

temp.df <- iso.df[c("location","depth",long_cols)] %>%
  pivot_longer(long_cols)

temp.df$name <- factor(temp.df$name,levels=long_cols,labels=label_list)

ggplot(temp.df, aes(x=depth, y=value, fill=location, color=location, shape=location))+
  mytheme

ggsave("figures/isotopes.png",width=mywidth, height=myheight)



# elemental ratios --------------------------------------------------------

long_cols <- c("CN","NP","d15N")
label_list <- c(bquote("C:N"~"Ratio"),
                bquote("N:P"~"Ratio"),
                bquote(delta^15*N~'(‰)'))

temp.df <- iso.df[c("location","depth",long_cols)]

for (row in 1:nrow(temp.df)){
  if (temp.df[row,"NP"]<0 | temp.df[row,"NP"]>40){
    temp.df[row,"NP"] <- NA
  }
  if (temp.df[row,"CN"]>40){
    temp.df[row,"CN"] <- NA
  }
}

temp.df <- temp.df %>%
  pivot_longer(long_cols) %>%
  drop_na(value)

temp.df$name <- factor(temp.df$name,levels=long_cols,labels=label_list)

ggplot(temp.df, aes(x=depth, y=value, fill=location, color=location, shape=location))+
  mytheme

ggsave("figures/element_ratios.png",width=mywidth, height=myheight)


# elemental composition again ---------------------------------------------

long_cols <- c("C.org","N","P.total")
label_list <- c(bquote(C[Organic]~'(%)'),
                bquote(N~'(%)'),
                bquote(P[Total]~x~10^-2~'(%)'))

temp.df <- iso.df[c("location","depth",long_cols)]

for (row in 1:nrow(temp.df)){
  if (temp.df[row,"C.org"]>10){
    temp.df[row,"C.org"] <- NA
  }
  if (temp.df[row,"N"]>0.7){
    temp.df[row,"N"] <- NA
  }
}

temp.df <- temp.df %>%
  pivot_longer(long_cols) %>%
  drop_na(value)

temp.df$name <- factor(temp.df$name,levels=long_cols,labels=label_list)

ggplot(temp.df, aes(x=depth, y=value, fill=location, color=location, shape=location))+
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

for (row in 1:nrow(mysizes.df)){
  for (row2 in 1:nrow(grainsizes.df)){
    if (mysizes.df[row,"Micrometers"]<grainsizes.df[row2,"Micrometers"]){
      mysizes.df[row,"Class"] <- grainsizes.df[row2,"Class"]
      break
    }
  }
}

grain.df <- left_join(grain.df,mysizes.df)

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
scale_color_viridis_d(option="H")+
scale_fill_viridis_d(option="H")+
scale_shape_manual(values=c(21:24))

ggsave("figures/grainsize.png",width=mywidth, height=myheight)
