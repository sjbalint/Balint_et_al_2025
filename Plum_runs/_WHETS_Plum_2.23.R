library(rplum)
library(tidyverse)

source("scripts/forplum.R")
source("scripts/agedepth.R")

source("scripts/graphing/configure_graphing.R")


# function for running plum models ---------------------------------------

run_plum <- function(name){
  
  Plum(core=name,ra.case=c(2),otherdates=paste0(name,"_Cs137.csv"),
       BCAD = TRUE, remove.tail = FALSE, n.supp = 0,
       ask = FALSE,
       age.lab = "Year",
       cal.col = rgb(207/255, 68/255, 70/255),
       cal.border = "black",
       mn.col="black", mn.lty=1, mn.lwd=2,
       prior.fontcol=rgb(120/255, 28/255, 109/255),
       prior.col=rgb(120/255, 28/255, 109/255))
}


# run plum models ---------------------------------------------------------

run_plum("Northern")

run_plum("Middle")

run_plum("Southern")

df1 <- info$detsOrig %>%
  mutate(name="210Pb")

colnames(df1) <- c("labID","depth.cm","density.gcm3","value","sd","thickness.cm", "name")

df2 <- info$supportedData %>%
  mutate(name="226Ra")

colnames(df2) <- c("value","sd","depth.cm","thickness.cm","name")

df <- bind_rows(df1,df2) %>%
  mutate(ymin = value - sd,
         ymax = value + sd,
         xmin = depth.cm,
         xmax = depth.cm + thickness.cm)

ggplot(df)+
  basetheme+
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=name), color="black", alpha=0.5)
