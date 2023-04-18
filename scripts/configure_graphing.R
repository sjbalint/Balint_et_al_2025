
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)


# create default graphing theme -------------------------------------------

basetheme <- list(
  theme_classic(),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.top = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "top"),
  scale_x_continuous(position = "top")
)

save(basetheme,file="Rdata/basetheme.Rdata")


# create a dataframe for axis labels --------------------------------------

ylabels.df <- data.frame(name=c('location','depth.cm','%N', "d15N.permil", "%C.total",
                                'd13C.total',"%C.organic",'d13C.organic',"n","P.pct.inorg",
                                "P.total.pct.e2", "P.pct.org","N.P.ratio","C.N.ratio",
                                "SiO2.prct","Si.P.ratio", "Si.N.ratio",
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
                             bquote("Si:P"~"Ratio"),
                             bquote("Si:N"~"Ratio"),
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

save(ylabels.df,file="Rdata/graphing_labels.Rdata")