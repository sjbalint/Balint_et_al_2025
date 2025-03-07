
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)

# import data -------------------------------------------------------------

data.df <- readRDS("Rdata/compiled_data.rds")

model.df <- readRDS("Rdata/segmented_model.rds")

changepoints.df <- readRDS("Rdata/segmented_changepoints.rds") %>%
  drop_na(breakpoint)

data.df <- data.df %>%
  filter(outlier==FALSE)

# graphing parameters -----------------------------------------------------

aspect_ratio=8/10

mywidth=105*2

myheight=mywidth*aspect_ratio

legend_title <- NULL

source("scripts/graphing/configure_graphing.R")


# functions for plotting --------------------------------------------------

add_ylabels <- function(df, ylabels.df){
  
  df <- left_join(df,ylabels.df)
  
  factor_names <- df %>%
    pull(factor1) %>%
    unique()
  
  df <- df %>%
    mutate(factor =factor(factor1,levels=factor_names),
           name = factor(name))
  
  return(df)
}

plot_longer <- function(data.df, model.df, long_cols){
  
  plot.df <- data.df %>%
    select(all_of(c("year.mean", "location", "below_pb_LOD", long_cols))) %>%
    pivot_longer(long_cols)
  
  model.df <- model.df %>%
    filter(name %in% long_cols)
  
  plot.df <- full_join(plot.df, model.df)
  
  plot.df <- add_ylabels(plot.df, ylabels.df)
  
  return (plot.df)
}

make_plot <- function(long.df, changepoints.df){
  
  points.df <- long.df %>%
    drop_na(value)
  
  model.df <- long.df %>%
    drop_na(fit) %>%
    select(year.mean, fit, fit.lower, fit.upper, factor) %>%
    unique()
  
  factor_list <- unique(long.df$factor)
  
  changepoints.df <- left_join(changepoints.df,ylabels.df) %>%
    filter(factor1 %in% factor_list) %>%
    mutate(factor=factor(factor1, levels=factor_list))
  
  filled_points.df <- points.df %>%
    filter(below_pb_LOD==FALSE)
  
  empty_points.df <- points.df %>%
    filter(below_pb_LOD==TRUE)
  
  p1 <- ggplot()+
    basetheme+
    geom_rect(data=changepoints.df,
              aes(xmin=-Inf, xmax=Inf,
                  ymin = year.lower, ymax=year.upper),
              fill="grey50", alpha=0.2)+
    geom_ribbon(data=model.df,
                aes(y=year.mean, x=fit, xmin=fit.lower, xmax=fit.upper),
                fill="grey50", alpha=0.2)+
    geom_point(data=filled_points.df,
               aes(y=year.mean, x=value, fill=location, shape=location),
               alpha=0.8, size=2.5, color="black")+
    geom_point(data=empty_points.df,
               aes(y=year.mean, x=value, shape=location),
               alpha=0.8, size=2.5, color="black", fill="white", show.legend=FALSE)+
    geom_line(data=model.df,
              aes(y=year.mean, x=fit),
              linewidth=1, orientation="y")+
    geom_hline(data=changepoints.df,
               aes(yintercept = year.mean),
               linetype="dashed")+
    facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "top",labeller = label_parsed)+
    labs(x=NULL,y="Year",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title)+
    scale_y_continuous(breaks=c(2000,1900,1800,1700),
                       labels=c("2000","1900","1800","Before\n1800"),
                       expand=expansion(mult=c(0.05,0.05)))+
    theme(legend.position="top",
          plot.margin = unit(c(0.25,0.25,1,0.25), "cm"))
  
  return(p1)
  
}

# grainsize ---------------------------------------------------------------

temp.df <- plot_longer(data.df, model.df,
                       c("mean.phi","sd.phi","accretion.rate.gcm2yr"))

make_plot(temp.df, changepoints.df)

ggsave("figures/Fig5.pdf",width=mywidth, height=myheight, units="mm")

# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,model.df,
                       c("C.N.ratio","C.P.ratio", "N.P.ratio"))

line_factors <- temp.df$factor %>%
  unique()

lines.df <- data.frame(factor=line_factors,x=c(NA,NA,16))

make_plot(temp.df, changepoints.df)+
  geom_vline(data=lines.df,aes(xintercept=x),linetype="dashed")

ggsave("figures/Fig6.pdf",width=mywidth, height=myheight, units="mm")

# isotopes ----------------------------------------------------------------

temp.df <- plot_longer(data.df, model.df,
                       c("d15N.permil","SiO2.prct","d13C.organic"))

#because default ggsave can't handle complex unicode characters
cairo_pdf("figures/Fig7.pdf",width=mywidth/25.4, height=myheight/25.4)

make_plot(temp.df, changepoints.df)

dev.off()

# elemental composition ---------------------------------------------------

temp.df <- plot_longer(data.df, model.df,
                       c("%C.organic","%N","P.total.pct.e2"))



make_plot(temp.df, changepoints.df)

ggsave("figures/FigS5.pdf",width=mywidth, height=myheight, units="mm")

