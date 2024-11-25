
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)

# import data -------------------------------------------------------------

data.df <- read_excel("raw/raw_dating.xlsx") %>%
  as.data.frame() %>%
  mutate(location=factor(location,levels=c("North","Middle","South")))

load("Rdata/dating.Rdata")

dating.df <- dating.df %>%
  filter(depth.cm>=0) %>%
  mutate(location=factor(location,levels=c("North","Middle","South")))


# end pb ------------------------------------------------------------------

result.list <- list()

for (mylocation in unique(data.df$location)){
  
  end.pb <- FALSE
  
  df <- dating.df %>%
    filter(location==mylocation) %>%
    arrange(depth.cm)
  
  for (row in 1:nrow(df)){
    
    if (!is.na(df[row,"end.pb"])){
      end.pb <- TRUE
    }
    
    df[row,"end.pb"] <- end.pb
    
  }
  
  result.list <- append(result.list, list(df))
  
}

dating.df <- bind_rows(result.list)

# graphing parameters -----------------------------------------------------

mywidth=6
myheight=10

legend_title <- NULL

source("scripts/graphing/configure_graphing.R")

plot_longer <- function(data.df,long_cols){
  plot.df <- data.df %>%
    pivot_longer(long_cols)
  
  plot.df <- left_join(plot.df,ylabels.df)
  
  factor_names <- plot.df %>%
    pull(factor1) %>%
    unique()
  
  plot.df <- plot.df %>%
    mutate(factor =factor(factor1,levels=factor_names),
           name=factor(name))
  
  return (plot.df)
}

# dating ---------------------------------------------------------------

fill.df <- plot_longer(data.df,c("137Cs_activity.bqkg","210Pb_excess.bqkg")) %>%
  select(c("location","depth.cm","factor","value"))

error.df <- plot_longer(data.df,c("137Cs_uncertainty.bqkg","210Pb_uncertainty.bqkg")) %>%
  select(c("location","depth.cm","depth.min","depth.max","factor","value")) %>%
  rename("uncertainty"="value")

fill.df <- full_join(fill.df,error.df) %>%
  mutate(max=value+uncertainty,
         min=value-uncertainty,
         vline=0)

year.df <- plot_longer(dating.df,c("year.mean")) %>%
  select(c("location","depth.cm","factor","value"))

error.df <- dating.df %>%
  select(location, depth.cm, year.min, year.max, end.pb) %>%
  rename(min=year.min, max=year.max)

year.df <- left_join(year.df, error.df)

year.df <- bind_rows(fill.df, year.df)

ggplot(year.df)+
  basetheme+
  geom_area(data=fill.df, aes(y=depth.cm,x=value, fill=location),orientation="y", alpha=0.3)+
  geom_line(aes(y=depth.cm,x=value),orientation="y")+
  geom_errorbarh(aes(x=value,y=depth.cm,
                     xmin=min, xmax=max),alpha=0.8)+
  geom_point(aes(y=depth.cm, x=value, fill=location, shape=location),
           size=2.5,color="black",alpha=0.7)+
  facet_grid(location~factor,scales="free_x",labeller = label_parsed)+
  labs(x=NULL,y="Depth\ncm",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title)+
  scale_shape_manual(values=c(21:24))+
  theme(legend.position="none")+
  scale_y_reverse()+
  geom_vline(data=fill.df, aes(xintercept=vline))

ggsave("figures/S6.png",width=12, height=10)

