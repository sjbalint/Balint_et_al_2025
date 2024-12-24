
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(scales) #for parse_format()

source("scripts/graphing/configure_graphing.R")

# import data -------------------------------------------------------------

data.df <- readRDS("Rdata/dating.rds")

endpb.df <- data.frame(location=as.factor(c("North","Middle","South")),
                       end.pb = c(38,38,42))

# graphing parameters -----------------------------------------------------

aspect_ratio=8/10

mywidth=105*2

myheight=mywidth*aspect_ratio

legend_title <- NULL

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


# format data for the plot ------------------------------------------------

#137Cs data
cs.df <- data.df %>%
  select(location, depth.cm, thickness.cm, Cs137_activity.bqkg, Cs137_uncertainty.bqkg)

colnames(cs.df) <- c("location","depth.cm","thickness.cm","value","uncertainty")

cs.df <- cs.df %>%
  filter(value>0) %>%
  mutate(name = "Cs137_activity.bqkg",
         activity = name)

cs.df <- left_join(cs.df, ylabels.df)

pb.df <- data.df %>%
  select(location, depth.cm, thickness.cm, Pb210.bqkg, Ra226.bqkg) %>%
  pivot_longer(cols=c("Pb210.bqkg", "Ra226.bqkg"),names_to="activity")

pb_error.df <- data.df %>%
  select(location, depth.cm, thickness.cm, Pb210_sd.bqkg, Ra226_sd.bqkg) %>%
  rename(Pb210.bqkg=Pb210_sd.bqkg, Ra226.bqkg=Ra226_sd.bqkg) %>%
  pivot_longer(cols=c("Pb210.bqkg", "Ra226.bqkg"), names_to="activity", values_to = "uncertainty")
  
pb.df <- left_join(pb.df, pb_error.df) %>%
  mutate(name="Pb210.bqkg")

pb.df <- left_join(pb.df, ylabels.df)

pb.df <- left_join(pb.df, endpb.df)

plot.df <- bind_rows(cs.df, pb.df) %>%
  mutate(activity=factor(activity,
                     labels=c(bquote(scriptstyle(atop(137,))*"Cs"),
                              bquote(scriptstyle(atop(210,))*"Pb"),
                              bquote(scriptstyle(atop(226,))*"Ra"))))

year.df <- data.df %>%
  select(location, depth.cm, year.min, year.max, year.mean) %>%
  mutate(factor1="Year")

year.df <- left_join(year.df, endpb.df) %>%
  mutate(plot.pb=ifelse(depth.cm>end.pb, FALSE,TRUE))

ggplot(plot.df)+
  basetheme+
  geom_hline(aes(yintercept=end.pb), linetype="dashed")+
  geom_hline(data=year.df, aes(yintercept=end.pb), linetype="dashed")+
  geom_rect(aes(xmin=value-uncertainty,
                            xmax=value+uncertainty,
                            ymin=depth.cm,
                            ymax=depth.cm-thickness.cm,
                            fill=activity),
            color="black", alpha=0.8
            )+
  geom_ribbon(data=subset(year.df, plot.pb==TRUE), 
              aes(xmin=year.min, xmax=year.max, y=depth.cm), fill="grey", color="black")+
  geom_line(data=year.df, aes(x=year.mean, y=depth.cm), linewidth=1, linetype="11")+
  geom_line(data=subset(year.df, plot.pb==TRUE), aes(x=year.mean, y=depth.cm), linewidth=1)+
  facet_grid(location~factor1, scales="free_x",labeller = label_parsed)+
  scale_y_reverse()+
  scale_fill_viridis_d(option="mako", direction = -1, end=0.9, labels = parse_format())+
  labs(x=NULL,y="Depth (cm)")+
  theme(panel.grid.major = element_line(color="grey"),
        legend.position="top",
        legend.title=element_blank())

ggsave("figures/Fig3.png",width=mywidth, height=myheight, units="mm")

