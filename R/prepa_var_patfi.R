## PREPARATION DES VARIABLES POUR IMPUTATION DU PATRIMOINE FINANCIER ##

library(Hmisc)
library(reshape)
library(StatMatch)
library(ggplot2)
library(MASS)
library(rms)


load(patIndFil)
load(patTraFil)
load(patMenFil)
load(erfMenFil)
load(erfIndFil)
load(menm)

## Preparing patrimoine dataframe for imputation
# build patrimoine dataframe

cats = c('01', '02','03','04','05','06','07','08','09','10','11','12')
bins = c( 0,3000,7500,15000,30000,45000,75000,105000,150000,225000,300000,450000,5000000)
pat <- subset(menage04, select=c(ident,sexepr,prona, prona2, agepr,occuppr,occupcj,diplopr,patfi, rminter, typmen, tu, pond))
rm(menage04)

# description de la variable PATFI
# Montant du patrimoine financier (en tranche)
# Sans objet ou valeur manquante
# 01 Moins de 3 000 ???
# 02 De 3 000 ??? à moins de 7 500 ???
# 03 De 7 500 ??? à moins de 15 000 ???
# 04 De 15 000 ??? à moins de 30 000 ???
# 05 De 30 000 ??? à moins de 45 000 ???
# 06 De 45 000 ??? à moins de 75 000 ???
# 07 De 75 000 ??? à moins de 105 000
# 08 De 105 000 ??? à moins de 150 000 ???
# 09 De 150 000 ??? à moins de 225 000 ???
# 10 De 225 000 ??? à moins de 300 000 ???
# 11 De 300 000 ??? à moins de 450 000 ???
# 12 450 000 ??? ou plus
# 98 Refus de répondre
# 99 Ne sait pas
table(pat$patfi, useNA='ifany')

## on vire les cat 98-99 
pat$patfi[pat$patfi %in% c('','98','99')] <- NA
table(pat$patfi, useNA = 'ifany')

## PREPARATION DE LA VARIABLE PATRMINTER
##on vire les 98-99
## change les hauts patrimoines en 12 pr matching avec erf car pas de cat 13 dans base ERF
pat$rminter[pat$rminter=='98'] <- NA
pat$rminter[pat$rminter=='99'] <- NA
pat$rminter[pat$rminter==''] <- NA
# pat$rminter[pat$rminter=='13'] <- '12'
table(pat$rminter, useNA='ifany')
str(pat$rminter)
# DEBINAGE DE RMINTER DANS LA BASE PATRIMOINE
cats = c('01', '02','03','04','05','06','07','08','09','10','11','12','13')
seuils = c( 0,2500,5000,7500,9500,12000, 14500, 20000, 25000, 30000, 36000, 48000, 72000, 215600)
pat$rminterdeb <- debin(pat$rminter, categories = cats, bins = seuils)
## log
pat$logrminterdeb <- log(pat$rminterdeb)
describe(pat$rminterdeb)
## ne produit pas de NA's


dont_keep_NAs <- TRUE

if (dont_keep_NAs) {
  pat2 <- subset(pat, !is.na(pat$patfi))
  pat3 <- subset(pat2, !is.na(pat2$rminter))
  pat <- pat3
  rm(pat2)
  rm(pat3)
}
summary(pat)

## linéarisation du patrimoine financier##
# pat$pat_new <- debin(pat$patfi, categories = cats, bins = bins )
pat$patfi_new <- debin2(pat$patfi, categories = cats, bins = bins )
summary(pat$patfi_new)
## log
pat$logpatfi_new <- log(pat$patfi_new)

## redéfinition des variables occuppr, occupcj, et diplopr pour correspondance entre les deux tables ##
pat$diplopr[pat$diplopr==1] <- 0
pat$diplopr[pat$diplopr==5] <- 4
table(pat$diplopr)
# TODO: mettre des within ci-dessous

pat <- within(pat, {
  occuppr[occuppr==3|occuppr==7|occuppr==8 ] <- 11
  occuppr[occuppr==2] <- 3
  occuppr[occuppr==4] <- 1
  occuppr[occuppr==5|occuppr==6] <- 4
  occuppr[occuppr==11] <- 5
})

pat <-  within(pat, {
  occupcj[occupcj==3|occupcj==7|occupcj==8 ] <- 11
  occupcj[occupcj==2] <- 3
  occupcj[occupcj==4] <- 1
  occupcj[occupcj==5|occupcj==6] <- 4
  occupcj[occupcj==11] <- 5
})

pat$occupcj[pat$occupcj==''] <- 0
table(pat$occupcj, useNA='ifany')


# Recode patrimoine typmen to match erf typmen coding
# 1 Personne seule
# 2 Couple sans enfant -> 3
# 3 Couple avec 1 enfant -> 4
# 4 Couple avec 2 enfants -> 4
# 5 Couple avec 3 enfants ou plus -> 4
# 6 Famille monoparentale -> 2
# 7 Autre cas -> 5

pat <- within(pat, {
  typmen[typmen==3 | typmen==5] <- 4
  typmen[typmen==2] <- 3
  typmen[typmen==6] <- 2
  typmen[typmen==7] <- 5
})

# rename tranche urbaine var
# TU Variable calculée
# Taille urbaine
# 1 Commune rurale
# 2 Moins de 20 000 habitants
# 3 De 20 000 à 100 000 habitants
# 4 Plus de 100 000 habitants
# 5 Agglomération parisienne hors Paris
# 6 Ville de Paris

# recoding tranche urbaine (unité urbaine de paris: agglo + ville de paris)
pat$tu[pat$tu==6] <- 5
pat$tu99 <- pat$tu


## categorisation de l'age

cats = c('01','02','03','04','05','06','07','08')
bins = c( 0,20,30,40,50,60,70,80,120)
pat$agepr_cat <- categorize(pat$agepr, seuil= bins, categories = cats)

## passage en facteurs
pat$occuppr <- as.factor(pat$occuppr)
pat$occupcj <- as.factor(pat$occupcj)
pat$sexepr <- as.factor(pat$sexepr)
pat$diplopr <- as.factor(pat$diplopr)
pat$typmen <- as.factor(pat$typmen)
pat$rminter <- as.factor(as.ordered(pat$rminter))
pat$rminterdeb <-as.numeric(pat$rminterdeb)
pat$agepr_cat <- as.factor(pat$agepr_cat)
pat$tu99 <- as.factor(pat$tu99)

#################################################################################################################
########################################################################################################
# building subset from erf to impute patrimoine

erf <- subset(menagem, select= c(ident, spr, agepr, ddipl, acteu5pr, acteu5cj, ztsam, zperm, zragm, zricm, zrncm, typmen5,tu99, wprm))
rm(menagem)

# build categories of rev_men
erf$rev_men <- erf$ztsam+ erf$zperm + erf$zragm + erf$zricm + erf$zrncm
erf$ztsam <- NULL
erf$zperm <- NULL
erf$zragm <- NULL
erf$zricm <- NULL
erf$zrncm <- NULL
cats = c('01', '02','03','04','05','06','07','08','09','10','11','12','13')
seuils = c( 0,2500,5000,7500,9500,12000, 14500, 20000, 25000, 30000, 36000, 48000, 72000)
# 01 Moins de 2 500 ???
# 02 De 2 500 ??? à moins de 5 000 ???
# 03 De 5 000 ??? à moins de 7 500 ???
# 04 De 7 500 ??? à moins de 9 500 ???
# 05 De 9 500 ??? à moins de 12 000 ???
# 06 De 12 000 ??? à moins de 14 500 ???
# 07 De 14 500 ??? à moins de 20 000 ???
# 08 De 20 000 ??? à moins de 25 000 ???
# 09 De 25 000 ??? à moins de 30 000 ???
# 10 De 30 000 ??? à moins de 36 000 ???
# 11 De 36 000 ??? à moins de 48 000 ???
# 12 De 48 000 ??? à moins de 72 000 ???
# 13 72 000 ??? ou plus
# 98 Refus de répondre
# 99 Ne sait pas

erf$rev_men_cat <- categorize(erf$rev_men, seuil=seuils, categories=cats )

#erf$rev_men <- NULL
# table(erf$rev_men_cat, useNA="ifany")

erf <- within(erf, {
  ddipl[ddipl==7] <- 0
  ddipl[ddipl==6] <- 11
  ddipl[ddipl==1] <- 7
  ddipl[ddipl==3] <- 6
  ddipl[ddipl==5] <- 2
  ddipl[ddipl==11] <- 3
})

table(erf$ddipl)

describe(pat$diplopr)
describe(erf$ddipl)

##harmonisation des cat de diplomes entre les deux bases ##
"Diplôme non déclaré
1 Diplôme supérieur --> 7
3 Baccalauréat + 2 ans --> 6
5 CAP, BEP ou autre diplôme de ce niveau --> 2
6 Brevet des collèges --> 3
7 Aucun diplôme ou CEP --> 0 

0 Sans diplôme
1 CEP, DFEO --> all 1 become 0
2 CAP, BEP
3 BEPC
4 Bac technique 
5 Bac général --> 4
6 1er cycle universitaire, DUT, BTS
7 2 ème et 3ème cycle, grandes écoles" 


# travaille sur les var Occuppr, cj et acteu5cj et pr
"La variable ACTEU5 est une recomposition de l'activité au sens BIT52 de la personne en 5 modalités :
  '1' = « salarié »,
'2' = « indépendant »,
'3' = « chômeur »,
'4' = « retraité »,
'5' = « autre inactif »,
calculée à partir des variables de l' EEC : ACTEU6 et STATUT et de l'ERFS-EEC : CS8COR.
La variable ACTEU5 est dans la table INDIVI06. Les variables ACTEU5PR et ACTEU5CJ (pour la personne de référence et le conjoint) sont dans la table MENAGE06."

erf$acteu5pr[erf$acteu5pr==2] <- 1
erf$acteu5cj[erf$acteu5cj==2] <- 1
table(erf$acteu5pr, useNA='ifany')
table(erf$acteu5cj, useNA='ifany')
describe(erf$acteu5pr)
describe(erf$acteu5cj)
erf$acteu5cj[is.na(erf$acteu5cj)] <- 0 


## categorisation de l'age
cats = c('01','02','03','04','05','06','07','08')
bins = c( 0,20,30,40,50,60,70,80,120)
erf$agepr_cat <- categorize(erf$agepr, seuil= bins, categories = cats)


## recoding tu99
# rename tranche urbaine var
# TU Variable calculée
# Taille urbaine
# 1 Commune rurale
# 2 Moins de 20 000 habitants
# 3 De 20 000 à 100 000 habitants
# 4 Plus de 100 000 habitants
# 5 Agglomération parisienne hors Paris 
# 6 Ville de Paris --> 5

table(erf$tu99, useNA='ifany')

# Tranche d'unité urbaine en 9 postes
# 0 Commune rurale --> 1 
# 1 Unité urbaine de moins de 5 000 habitants --> 2
# 2 Unité urbaine de 5 000 à 9 999 habitants --> 2
# 3 Unité urbaine de 10 000 à 19 999 habitants --> 2
# 4 Unité urbaine de 20 000 à 49 999 habitants --> 3
# 5 Unité urbaine de 50 000 à 99 999 habitants --> 3
# 6 Unité urbaine de 100 000 à 199 999 habitants --> 4
# 7 Unité urbaine de 200 000 à 1 999 999 habitants --> 4
# 8 Unité urbaine de Paris --> 5

table(erf$tu99, useNA='ifany')

erf <- within(erf, {
  tu99[tu99==1|tu99==3] <- 2
  tu99[tu99==4|tu99==5] <- 3
  tu99[tu99==6|tu99==7] <- 4
  tu99[tu99==0] <- 1
  tu99[tu99==8] <- 5
})

table(erf$tu99, useNA='ifany')

## Pour effectuer le predict - il nous faut les mêmes noms de variables dans le newdata ##
## on renomme les vr 
erf$occuppr <- erf$acteu5pr
erf$occupcj <- erf$acteu5cj
erf$sexepr <- erf$spr
erf$diplopr <- erf$ddipl
erf$rminter <- erf$rev_men_cat
erf$rminterdeb <-erf$rev_men
erf$logrminterdeb <- log(erf$rev_men)
erf$typmen <- erf$typmen5
table(erf$occuppr, useNA='ifany')
table(erf$occupcj, useNA='ifany')

erf$typmen5 <- NULL
erf$acteu5pr<- NULL
erf$acteu5cj<- NULL
erf$spr <- NULL
erf$ddipl <- NULL
erf$rev_men_cat <- NULL

erf$occuppr <- as.factor(erf$occuppr)
erf$occupcj <- as.factor(erf$occupcj)
erf$sexepr <- as.factor(erf$sexepr)
erf$diplopr <- as.factor(erf$diplopr) 
erf$typmen <- as.factor(erf$typmen)
erf$agepr_cat <- as.factor(erf$agepr_cat)
erf$tu99 <- as.factor(erf$tu99)

# TODO: should it be ordered ?
erf$rminter <-as.factor(as.ordered(erf$rminter))

## BOXCOX
bc <- boxcox(patfi_new~ sexepr + agepr + I(agepr**2) + diplopr + occuppr + occupcj + rminter + typmen + tu99 , data=pat)
##
# Assume "bc" is an object returned by boxcox(...), you can do
## value for lambda
with(bc, x[which.max(y)])

## CHOIX DU LOG: lambda= 0.060

