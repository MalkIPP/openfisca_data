rm(list = ls())
library(foreign)
library(Hmisc)
library(car)      # provides recode function

RmAttributes <- function(data){
  for (v in names(data)){
    var <- data[[v]]
    if (is.factor(var)){
      data[[v]] <- levels(var)[var]
    }
    if (is.character(data[[v]]) & sum(!(is.na(data[[v]]) | grepl("^[0-9]+$",data[[v]])))==0){
      data[[v]] <- as.numeric(data[[v]])
    }
    attributes(data[[v]]) <- NULL
  }
  data
}

RenameVars <- function(data,yr){
# import from sas changed '_' in '.' in varnames. Those are not valid variable names in Octave.
# leading dots changes in 'f', other dots in '_'
  fnames <- names(data)
  varsOld1 <- fnames[grep(glob2rx("*.*"),fnames)]
  varsNew1 <- sub(glob2rx(".*"),"f" ,varsOld1)
  varsNew1 <- sub("\\.",'_' ,varsNew1)
  names(varsNew1) <- varsOld1
  varsOld2 <- fnames[grep(glob2rx(paste('*',yr,sep='')),fnames)]
  varsNew2 <- sub(yr,'' ,varsOld2)
  names(varsNew2) <- varsOld2
  varsNew <- c(varsNew1,varsNew2)
  data <- upData(data, rename = varsNew)
}

LoadSasTable <- function(files,dir,sasloc,yr){
  for (file in files){
    data <- sas.get(dir,mem=file,formats=FALSE,sasprog=sasloc)
    data <- RmAttributes(data)
    data <- RenameVars(data,yr)
    a <- file
    assign(a,data)
    saved_file <- paste(file,".Rdata",sep="")
    save(list = a,file=saved_file)
    rm(a)
    gc()
  }
}

LoadStataTableERF <- function(files,dir,yr){
  for (file in files){
    file_dta <- paste(dir,"/",file,".dta",sep="")
    print(file_dta)
    data <- read.dta(file_dta)
    data <- RmAttributes(data)
    data <- RenameVars(data,yr)
    a <- file
    assign(a,data)
    saved_file <- paste(file,".Rdata",sep="")
    save(list = a,file=saved_file)
    rm(a,data)
    gc()
  }
}


LoadStataTable <- function(files,dir){
  for (file in files){
    file_dta <- paste(dir,file,".dta",sep="")
    data <- read.dta(file_dta)
    a <- file
    assign(a,data)
    saved_file <- paste(file,".Rdata",sep="")
    save(list = a,file=saved_file)
    rm(a,data)
    gc()
  }
}





year = '2008';
year1 = '2009';
yr  <- substring(year,first=nchar(year)-1,last=nchar(year))
yr1 <- substring(year1,first=nchar(year)-1,last=nchar(year))

## Location of sas.exe
#sasloc ="C:/Program Files/SAS/SAS System/9.0/sas.exe"
sasloc = "C:/Program Files (x86)/SAS/SAS System/9.0/sas.exe"

## Fichiers sources
masterDataDir <- "C:/Users/Utilisateur/Documents/Data/"

## Dossier de sortie
outputDir <- "C:/Users/Utilisateur/Documents/Data/R/"

outputErfDir <- paste(outputDir,'erf/',sep="")
dir.create(paste(outputErfDir,year,sep=""), showWarnings = FALSE)
setwd(paste(outputErfDir,year,sep=""))
# 
erfFoyDir  <- paste(masterDataDir,"erf/",year,"/Foyers",sep="")
erfFoyFil  <- paste("foyer",yr,sep="")
LoadSasTable(c(erfFoyFil),erfFoyDir,sasloc,yr)

erfMenDir  <- paste(masterDataDir,"erf/",year,"/Menages",sep="")
erfMenFil <- paste("menage",yr,sep="")
eecMenFil <- paste("mrf",yr,"e",yr,"t4",sep="")
LoadSasTable(c(erfMenFil,eecMenFil),erfMenDir,sasloc,yr)

erfIndDir  <- paste(masterDataDir,"erf/",year,"/Individus",sep="")
erfIndFil <- paste("indivi",yr,sep="")
eecIndFil <- paste("irf",yr,"e",yr,"t4",sep="")

if (yr=='08') {
  # Pb with SAS table. eecIndFil converted to stata
  LoadSasTable(erfIndFil,erfIndDir,sasloc,yr)
  LoadStataTableERF(eecIndFil,erfIndDir,yr)
  } else {
  LoadSasTable(c(erfIndFil,eecIndFil),erfIndDir,sasloc,yr)
  }


erfCmpDir  <- paste(masterDataDir,"erf/",year,"/Tables complementaires",sep="")
eecCmp1Fil <-paste("icomprf",yr,"e",yr1,"t1",sep="")
eecCmp2Fil <-paste("icomprf",yr,"e",yr1,"t2",sep="")
eecCmp3Fil <-paste("icomprf",yr,"e",yr1,"t3",sep="")
if (yr=='08') {
  # Pb with SAS tables. 
  # yr = 08 eecCmp1Fil needs to be converted to stata 
  LoadSasTable(c(eecCmp2Fil,eecCmp3Fil),erfCmpDir,sasloc,yr)
  LoadStataTableERF(eecCmp1Fil,erfCmpDir,yr)
  } else {
  LoadSasTable(c(eecCmp1Fil,eecCmp2Fil,eecCmp3Fil),erfCmpDir,sasloc,yr)
  }

# outputLgtDir <- paste(outputDir,'logement/',sep="")
# print(paste(outputLgtDir,year,sep=""))
# setwd(paste(outputLgtDir,year,sep=""))
# 
# lgtDir     <- paste(masterDataDir,"logement/",year,"/enq_",yr,sep="")
# lgtAdrFil <- "adresse"
# if (yr=="03"){
#   lgtMenFil  <- "menage"
# }
# if (yr=="06"){
#   lgtMenFil  <- "menage1"
#   lgtLgtFil <- "logement"
#   LoadSasTable(c(lgtLgtFil),lgtDir,sasloc,yr)
# }
# LoadSasTable(c(lgtAdrFil,lgtMenFil),lgtDir,sasloc,yr)


# Build Rdata tables from enquete patrimoine as stata
# No dates for the time being
# year = "2004"
# yr = "04"
# outputPatDir <- paste(outputDir,'patrimoine/',sep="")
# print(paste(outputPatDir,year,sep=""))
# setwd(paste(outputPatDir,year,sep=""))
# PatDir <- paste(masterDataDir,"patrimoine/",year,"/",sep="")
# 
# patIndFil <- paste("individu",yr,sep="")  
# menIndFil <- paste("menage",yr,sep="")
# proIndFil <- paste("produit",yr,sep="")
# traIndFil <- paste("transm",yr,sep="")
# LoadStataTable(c(patIndFil,menIndFil,proIndFil,traIndFil),PatDir)


