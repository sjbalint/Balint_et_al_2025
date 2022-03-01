library(readxl)
library(tidyverse)

#import data
isotopes_2 <- read_excel("data/isotopes_2.xlsx") %>%
  group_by(Location,`Depth`) %>%
  summarize(`%N`=mean(`%N`),
            d15N=mean(d15N),
            `%C.total`=mean(`%C.total`),
            d13C.total=mean(d13C.total))

isotopes_2$Replicate <- "1"

isotopes_1 <- read_excel("data/isotopes_1.xlsx")
isotopes_1 <- isotopes_1[,c(2,3,7:9,11)]
colnames(isotopes_1) <- c("Location","Depth","d15N","%N","d13C.total","%C.total")
isotopes_1 <- isotopes_1 %>%
  drop_na("d15N")
isotopes_1$Replicate <- "2"


isotopes_3 <- read_excel("data/isotopes_3.xlsx") %>%
  group_by(Location,`Depth`) %>%
  summarize(`%N`=mean(`%N`),
            d15N=mean(d15N),
            `%C.total`=mean(`%C.total`),
            d13C.total=mean(d13C.total))

isotopes_3$Replicate <- "3"

all_isotopes <- rbind(isotopes_1,isotopes_2,isotopes_3) %>%
  filter(Location=="South")

theme_set(theme_bw())

subset <- all_isotopes[c("Replicate","Depth","d15N", "d13C.total")]

#subset <- subset(subset,subset$`%C.organic`<10)

subset <- pivot_longer(subset,cols=3:ncol(subset))

subset$name <- factor(
  subset$name,
  labels=c(
    bquote(delta^13*C~'(‰)'),
    bquote(delta^15*N~'(‰)')
  )
)

ggplot(subset, aes(x=Depth, y=value, group=Replicate))+
  geom_smooth(aes(color=Replicate), alpha=0.5, se=FALSE)+
  geom_point(shape=21, size=2.5, aes(fill=Replicate))+
  coord_flip()+
  #scale_x_reverse(labels=function(x)2020-(x*3))+
  scale_x_reverse()+
  facet_wrap(~name,
             nrow=1,
             scales="free_x",
             strip.position = "bottom",
             labeller = label_parsed
  )+
  ylab(NULL) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
  )+
  xlab("Depth (cm)")+
  scale_color_manual(
    values=c(
      "#E69F00", "#999999", "#56B4E9"
    )
  )+
  scale_fill_manual(
    values=c(
      "#E69F00", "#999999", "#56B4E9"
    )
  )

ggsave("figures/isotope_comparison.png",width=8, height=4.5)

