library(rplum)

Plum(core="Northern",ra.case=c(2),otherdates="Northern_Cs137.csv", BCAD=TRUE, remove.tail = FALSE)

Plum(core="Middle",ra.case=c(2),otherdates="Middle_Cs137.csv", BCAD=TRUE, remove.tail = FALSE)

Plum(core="Southern",ra.case=c(2),otherdates="Southern_Cs137.csv",BCAD=TRUE)