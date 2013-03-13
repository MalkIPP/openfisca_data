# OpenFisca
# Declare table names and paths taking care of different years.

message('Entering 00_config');

yr <- substring(year,first=nchar(year)-1,last=nchar(year));
yr1 <- substring(year1,first=nchar(year)-1,last=nchar(year));

## Path configurations 
source("0000_path_config.R")

outputErfDir <- paste(outputDir,'erf/',sep="")
outputLgtDir <- paste(outputDir,'logement/',sep="")
outputPatDir <- paste(outputDir,'patrimoine/',sep="")


## Localisation of raw data for various years

# Enquete revenus fiscaux (et sociaux) ERF(S)
brutErfDir <- paste(outputErfDir,year,'/',sep="")
  
erfFoyFil <- paste(brutErfDir,"foyer",yr,".Rdata",sep="")
erfMenFil <- paste(brutErfDir,"menage",yr,".Rdata",sep="")
eecMenFil <- paste(brutErfDir,"mrf",yr,"e",yr,"t4",".Rdata",sep="")
erfIndFil <- paste(brutErfDir,"indivi",yr,".Rdata",sep="")
eecIndFil <- paste(brutErfDir,"irf",yr,"e",yr,"t4",".Rdata",sep="")
eecCmp1Fil <-paste(brutErfDir,"icomprf",yr,"e",yr1,"t1",".Rdata",sep="")
eecCmp2Fil <-paste(brutErfDir,"icomprf",yr,"e",yr1,"t2",".Rdata",sep="")
eecCmp3Fil <-paste(brutErfDir,"icomprf",yr,"e",yr1,"t3",".Rdata",sep="")

# Enquete logement

if (yr=="03"){
  year_lgt <- 2003
  brutLgtDir <- paste(outputLgtDir,year_lgt,'/',sep="")
  lgtMenFil  <- paste(brutLgtDir,"menage",".Rdata",sep="")
  renameidlgt  <- c(ident='ident')
}
if (yr %in%  c("06","07","08","09")){
  year_lgt <- 2006
  brutLgtDir <- paste(outputLgtDir,year_lgt,'/',sep="")
  lgtMenFil <- paste(brutLgtDir,"menage1",".Rdata",sep="")
  lgtLgtFil <- paste(brutLgtDir,"logement",".Rdata",sep="")
  renameidlgt  <- c(idlog='ident')
}

lgtAdrFil <- paste(brutLgtDir,"adresse",".Rdata",sep="")

# Enquete Patrimoine
brutPatDir <- paste(outputPatDir,year,'/',sep="")
patIndFil <- paste(brutPatDir,"individu",yr,".Rdata",sep="")  
patMenFil <- paste(brutPatDir,"menage",yr,".Rdata",sep="")
patProFil <- paste(brutPatDir,"produit",yr,".Rdata",sep="")
patTraFil <- paste(brutPatDir,"transm",yr,".Rdata",sep="")


# Table produced
openfiscaDir <- paste(outputDir,'openfisca/',year,'/',sep="")
tmpDir  <- paste(openfiscaDir,'tmp/',sep="")


menm <- paste(openfiscaDir,"menage",yr,"m",".Rdata",sep=""); # m for merged
indm <- paste(openfiscaDir,"indivi",yr,"m",".Rdata",sep="");
fipDat <- paste(openfiscaDir,"fip",yr,".Rdata",sep="");
foyc <- paste(openfiscaDir,"foyer",yr,".Rdata",sep="");     # c for computed
indc <- paste(openfiscaDir,"indivi",yr,".Rdata",sep="");
menc <- paste(openfiscaDir,"menage",yr,".Rdata",sep="");
famc <- paste(openfiscaDir,"famille",yr,".Rdata",sep="");
enfnnm <- paste(openfiscaDir,"enfnn",yr,"m",".Rdata",sep="")
