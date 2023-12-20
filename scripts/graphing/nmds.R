rm(list=ls())

# load packages -----------------------------------------------------------

library(vegan)
library(tidyverse)
library(grid)
library(ggsci)

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

# format plots ------------------------------------------------------------

theme_set(theme_classic())

mywidth=8
myheight=4.5

mytheme <- list(
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.bottom = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "right"),
  scale_fill_jco(),
  scale_color_jco(),
  scale_shape_manual(values=c(21:25))
)

# perform NMDS ------------------------------------------------------------

pred.list <- c("%N","d15N.permil","%C.organic","P.pct.total","SiO2.prct",
               "kurtosis.um","mean.phi", "sd.phi", "accretion.rate.gcm2yr")
resp.list <- c("year.mean")
cat.list <- c("location", "century")

data <- data.df %>%
  filter(outlier==FALSE) %>%
  select(all_of(c(pred.list,resp.list,cat.list))) %>%
  drop_na(all_of(c(pred.list,resp.list))) %>%
  mutate(ID=row_number())

# perform clustering ------------------------------------------------------

dist <- dist(data)
clust <- hclust(dist,method="average")
plot(clust)
smalltree <- cutree(clust,k=3)

data$cluster <- as.factor(smalltree)

mds <- metaMDS(data[c(pred.list)], trace = FALSE)
ef <- envfit(mds, data[c(resp.list)], permu = 999)

ef.df<-as.data.frame(ef$vectors$arrows*sqrt(ef$vectors$r))

ef.df$response <- c("Year")

stressplot(mds)

data.scores = as.data.frame(scores(mds,display="sites"))

# graph and save results --------------------------------------------------

cat.list <- c(cat.list, "cluster")

temp.df <- data[c("ID",resp.list,cat.list)]
data.scores$ID <- data$ID
data.scores <- left_join(data.scores,temp.df)

for (cat in cat.list){
  p1 <- ggplot()+
    geom_point(data=data.scores,aes(NMDS1,NMDS2,fill=get(cat), shape=get(cat)),
               size=2,color="black",alpha=0.5)+
    geom_segment(data=ef.df,aes(x=0,xend=NMDS1/2,y=0,yend=NMDS2/2),
                 arrow = arrow(length = unit(0.5, "cm")),colour="black",alpha=0.5) + 
    geom_text(data=ef.df,aes(x=NMDS1/1.8,y=NMDS2/1.8,label=response),parse=TRUE)+
    mytheme+
    labs(fill=cat,shape=cat)+
    coord_fixed()
  print(p1)
  ggsave(paste0("figures/NMDS/",cat,"_NMDS.png"),plot=p1,width=mywidth,height=myheight)
}

ggplot(data.scores, aes(year.mean, NMDS1, color=cluster, fill=cluster, shape=location))+
  mytheme+
  geom_line()+
  geom_point(color="black")

ggsave(paste0("figures/NMDS/clustered.year_NMDS.png"),width=mywidth,height=myheight)
  
