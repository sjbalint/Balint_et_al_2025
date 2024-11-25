library(rplum)
library(tidyverse)

source("scripts/forplum.R")
source("scripts/agedepth.R")

source("scripts/graphing/configure_graphing.R")

# northern core -----------------------------------------------------------

Plum(core="Northern",ra.case=c(2),otherdates="Northern_Cs137.csv", BCAD=TRUE, remove.tail = FALSE)

# middle core -------------------------------------------------------------

Plum(core="Middle",ra.case=c(2),otherdates="Middle_Cs137.csv", BCAD=TRUE, remove.tail = FALSE, n.supp=0)

# southern core -----------------------------------------------------------

Plum(core="Southern",ra.case=c(2),otherdates="Southern_Cs137.csv",BCAD=TRUE, remove.tail = FALSE, n.supp=0)

#likelihood that pb210 has reached background
background()
agedepth(age.lab="Year",
         cal.col =rgb(207/255, 68/255, 70/255),
         cal.border = "black",
         mn.col="black",
         mn.lty=1,
         mn.lwd=2,
         pbmodelled.col= function(x) rgb(0, 0, 1, 0.7 * x),
         supp.col = rgb(120/255, 28/255, 109/255),
         prior.fontcol=1,
         prior.col=rgb(120/255, 28/255, 109/255),
         pb.lty = 1,
         pbmeasured.col="black")

draw.pbmeasured(pbmeasured.col="black",
                pbmeasured.lty=1,
                newplot=TRUE)

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
