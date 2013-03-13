rm(list = ls())

library(foreign)
library(Hmisc)


RVersion =R.Version()
if (RVersion$os != "linux-gnu") {
  srcPikettyDir <- "C:/Users/Utilisateur/Desktop/Simulateur_Complet/Fichiers/"
  outputPikettyDir <- "C:/Users/Utilisateur/Documents/Data/R/"} else {
    outputPikettyDir <- "/home/benjello/economie/data/R/"}



year <- "2010"
pifPatFil <- paste(srcPikettyDir,"indiv_isf_",year,".dta",sep="")

year <- "2006"
pikIndFil <- paste(srcPikettyDir, "indiv_rev_", year, ".dta", sep="")
pikLogFil <- paste(srcPikettyDir, "indiv_logt_", year, ".dta", sep="")
ind6 <-read.dta(pikIndFil)
logt6 <-read.dta(pikLogFil)
summary(logt6$loyer_fictif)
## comment il simule les loyers fictifs? ##

patri <- read.dta(pifPatFil)
names(patri)
describe(patri$actifnetISF*patri$decl,weights=patri$pondv)
sum(patri$actifnetISF*patri$decl*patri$pondv)
sum(patri$ISF*patri$decl*patri$pondv)/1e9