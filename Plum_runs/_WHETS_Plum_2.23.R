library(rplum)
library(tidyverse)

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
