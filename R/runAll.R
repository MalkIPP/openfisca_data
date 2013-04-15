rm(list = ls())
gc()

library(Hmisc)
library(reshape)
library(car)

# SET THE WORKING DIRECTORY TO SOURCE FILE LOCATION 

# Starting the logging
log_connection <- file("erf_log.txt", open="wt")
sink(log_connection, split=TRUE)

# Load some useful functions
source("common.R")

#   START
#  year = '2008'
#  year1 = '2010'

date()
year = '2008'
year1 = '2009'

source('0000_path_config.R')

source('00_config.R')

source('01_pre_proc.R')

############################################################################
## work on menages tables
############################################################################

# Imputation des loyers
source('02_imput_loyer.R')

# at this point, every useful info is in file menm.

# Extract usefull variables
# loym (imputed), tu99, pol99, reg, tau99 (from eec), so, ... 

############################################################################

## Imputations des patrimoines 
#source('02_imput_patri.R')

## Imputations patfi
#source ('02_imput_biensprof_patfi.R')

## Imputations res princ
#source ('02_imput_residence_principale.R')

############################################################################
## Retrieving FIPs
############################################################################
source('03_fip.R')

############################################################################
## Construction of families
############################################################################
source('04_famille.R')

############################################################################
## Extracting data for foyers
############################################################################
source('05_foyer.R')

############################################################################
## Re-Construction of indiv, foyers and families
############################################################################
source('06_rebuild.R')

############################################################################
## Sets the inv for invalides variable
############################################################################
source('07_invalides.R ')

############################################################################
## Final miscallenous settings: recoding, merging to get rents, computing zone_apl, etc.
############################################################################
source('08_final.R ')

write.csv(final2, file=paste(openfiscaDir,"final",yr,".csv",sep=""), na = '0', row.names = FALSE)
gc()
sink()

