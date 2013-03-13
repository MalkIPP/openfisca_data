# OpenFisca
# Declare table names and paths taking care of different years.

message('Entering 00_config');
rm(list = ls());

library(Hmisc);
library(car);

LoadIn <- function(file,vars=NULL){
  tmp <- load(file);
  if (is.null(vars)){
    out <- get(tmp);}
  else{ 
    out <- get(tmp)[vars];
  }
}

###   START
year = '2004';
year1 = '2005';

yr <- substring(year,first=nchar(year)-1,last=nchar(year));
yr1 <- substring(year1,first=nchar(year)-1,last=nchar(year));

## Dossiers où se trouvent les diverses enquètes au format R 
RVersion =R.Version()
if (RVersion$os != "linux-gnu") {
  outputDir <- "C:/Users/Utilisateur/Documents/Data/R/"} else {
  outputDir <- "/home/benjello/economie/data/R/"}
  
outputErfDir <- paste(outputDir,'erf/',sep="")
outputLgtDir <- paste(outputDir,'logement/',sep="")
outputPatDir <- paste(outputDir,'patrimoine/',sep="")


openfiscaDir <- paste(outputDir,'openfisca/',year,'/',sep="")
tmpDir  <- paste(openfiscaDir,'tmp/',sep="")
brutPatDir <- paste(outputPatDir,year,'/',sep="")

patIndFil <- paste(brutPatDir,"individu",yr,".Rdata",sep="")  
patMenFil <- paste(brutPatDir,"menage",yr,".Rdata",sep="")
patProFil <- paste(brutPatDir,"produit",yr,".Rdata",sep="")
patTraFil <- paste(brutPatDir,"transm",yr,".Rdata",sep="")

### REMOVE FROM HERE TO STOP BECAUSE REDUNDANT

year = '2006';
year1 = '2007';

yr <- substring(year,first=nchar(year)-1,last=nchar(year));
yr1 <- substring(year1,first=nchar(year)-1,last=nchar(year));

## Dossiers où se trouvent les diverses enquètes au format R pour l'année considérée
brutErfDir <- paste(outputErfDir,year,'/',sep="")
brutLgtDir <- paste(outputLgtDir,year,'/',sep="")
openfiscaDir <- paste(outputDir,'openfisca/',year,'/',sep="")
tmpDir  <- paste(openfiscaDir,'tmp/',sep="")

erfFoyFil <- paste(brutErfDir,"foyer",yr,".Rdata",sep="");
erfMenFil <- paste(brutErfDir,"menage",yr,".Rdata",sep="");
eecMenFil <- paste(brutErfDir,"mrf",yr,"e",yr,"t4",".Rdata",sep="");
erfIndFil <- paste(brutErfDir,"indivi",yr,".Rdata",sep=""); 
eecIndFil <- paste(brutErfDir,"irf",yr,"e",yr,"t4",".Rdata",sep="");


saveTmp <- function(data, file = stop("'file' must be specified")){
  name <- deparse(substitute(data))
  path <- paste(tmpDir, file, sep = "")
  save(list = name, file = path)
}

loadTmp <- function(file){
  path <- paste(tmpDir, file, sep = "")
  load(path, globalenv())
}

delTmp <- function(){
  # deletes all files in tmpDir
  files <- dir(tmpDir)
  for (file in files){
    path <- paste(tmpDir, file, sep = "")
    file.remove(path)    
  }
}


# output tables
menm <- paste(openfiscaDir,"menage",yr,"m",".Rdata",sep=""); # m for merged
indm <- paste(openfiscaDir,"indivi",yr,"m",".Rdata",sep="");



stop("exiting to avoid loading erf and logement stuff (this is not an error")
#warning('exit')
##### STOP


## Dossiers où se trouvent les diverses enquètes au format R pour l'année considérée
brutErfDir <- paste(outputErfDir,year,'/',sep="")
brutLgtDir <- paste(outputLgtDir,year,'/',sep="")
openfiscaDir <- paste(outputDir,'openfisca/',year,'/',sep="")
tmpDir  <- paste(openfiscaDir,'tmp/',sep="")
  
erfFoyFil <- paste(brutErfDir,"foyer",yr,".Rdata",sep="");
erfMenFil <- paste(brutErfDir,"menage",yr,".Rdata",sep="");
eecMenFil <- paste(brutErfDir,"mrf",yr,"e",yr,"t4",".Rdata",sep="");
erfIndFil <- paste(brutErfDir,"indivi",yr,".Rdata",sep=""); 
eecIndFil <- paste(brutErfDir,"irf",yr,"e",yr,"t4",".Rdata",sep="");
eecCmp1Fil <-paste(brutErfDir,"icomprf",yr,"e",yr1,"t1",".Rdata",sep="");
eecCmp2Fil <-paste(brutErfDir,"icomprf",yr,"e",yr1,"t2",".Rdata",sep="");
eecCmp3Fil <-paste(brutErfDir,"icomprf",yr,"e",yr1,"t3",".Rdata",sep="");
lgtAdrFil <- paste(brutLgtDir,"adresse",".Rdata",sep="");
if (yr=="03"){
  lgtMenFil  <- paste(brutLgtDir,"menage",".Rdata",sep="");
  renameidlgt  <- c(ident='ident');
}
if (yr=="06"){
  lgtMenFil <- paste(brutLgtDir,"menage1",".Rdata",sep="");
  lgtLgtFil <- paste(brutLgtDir,"logement",".Rdata",sep="");
  renameidlgt  <- c(idlog='ident');
}
# output tables
menm <- paste(openfiscaDir,"menage",yr,"m",".Rdata",sep=""); # m for merged
indm <- paste(openfiscaDir,"indivi",yr,"m",".Rdata",sep="");
fipDat <- paste(openfiscaDir,"fip",yr,".Rdata",sep="");
foyc <- paste(openfiscaDir,"foyer",yr,".Rdata",sep="");     # c for computed
indc <- paste(openfiscaDir,"indivi",yr,".Rdata",sep="");
menc <- paste(openfiscaDir,"menage",yr,".Rdata",sep="");
famc <- paste(openfiscaDir,"famille",yr,".Rdata",sep="");

saveTmp <- function(data, file = stop("'file' must be specified")){
  name <- deparse(substitute(data))
  path <- paste(tmpDir, file, sep = "")
  save(list = name, file = path)
}

loadTmp <- function(file){
  path <- paste(tmpDir, file, sep = "")
  load(path, globalenv())
}

delTmp <- function(){
  # deletes all files in tmpDir
  files <- dir(tmpDir)
  for (file in files){
    path <- paste(tmpDir, file, sep = "")
    file.remove(path)    
  }
}

print_id <- function(dts){
  print("Individus")
  print(length(table(dts$noindiv)))
  
  # Ici, il doit y avoir autant de vous que d'idfoy
  print("Foyers")
  print(length(table(dts$idfoy)))
  print(table(dts$quifoy,useNA="ifany"))
  
  # Ici, il doit y avoir autant de quimen = 0 que d'idmen
  print("Ménages")
  print(length(table(dts$idmen)))
  print(table(dts$quimen,useNA="ifany"))
  
  # Ici, il doit y avoir autant de quifam = 0 que d'idfam TODO problÃ¨me
  print("Familles")
  print(length(table(dts$idfam)))
  print(table(dts$quifam,useNA="ifany"))
}
