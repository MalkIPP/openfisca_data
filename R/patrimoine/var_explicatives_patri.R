## travail sur les variables explicatives ## 
rm(list = ls())
RVersion =R.Version()
if (RVersion$os != "linux-gnu") {
  setwd("C:/Users/Utilisateur/Dropbox/python_project/mSim/R/patrimoine")
} else {
  setwd("/home/benjello/Dropbox/python_project/mSim/R/patrimoine")}
rm(RVersion)


library(Hmisc)
library(reshape)
library(xtable)


source('00_config.R')
source('debin.R')

load(patIndFil)
load(patTraFil)
load(patMenFil)
load(erfMenFil)
load(erfIndFil)
load(menm)
summary(irf06e06t4)
summary(menage06)
summary(menagem)
library(rms)
library(xtable)

sexeage <- xtabs(~patri + sexepr)
sexeage.table <- xtable(sexeage)
print(sexeage.table,floating=FALSE)


xtabs(~ agepr + sexepr)
plot(agepr, sexepr)
sexeage <- xtabs(~patri + sexepr)
patri <- as.numeric(patri)
hist(patri, breaks=100)

## OBSERVATIONS DES MENAGES 98/99 
## on supposera (voir mail avec madhi) que les 99 correspondent à des patrimoines faibles- tandis que les 98 à de haut patrimoines##
## dans un cas contraire: menage04$patri[menage04$patri=="98"] <- NA
## menage04$patri[menage04$patri=="99"] <- NA


## ETUDE DE SEXE ET AGEPR ##
detach(menage04)
## crée une colonne de 0 pour sexe et remplace les hommes (sexepr=1) ##
menage04$sexe[menage04$sexepr == 1] <- 1
menage04$sexe[menage04$sexepr == 2] <- 0
View(menage04$sexe)
attach(menage04)
ddistsexe<- datadist(sexe, agepr)
options(datadist='ddistsexe')
sexeage <- lrm(patri ~ sexe + agepr)
summary(sexeage)
sexeage.table <- xtable(summary(sexeage))
print(sexeage.table,floating=FALSE)

summary(sexeage)

detach(menage04)




## predicted probas for sexe (age fixed at mean) ## 
sexe <- (c(0,1))
agepr <-mean(menage04$agepr)
newdata1<-data.frame(sexe,agepr)
newdata1$predicted<-predict(sexeage,newdata=newdata1,type="fitted.ind")
newdata1
newdata1.table <- xtable(newdata1)
print(newdata1.table)

## ETUDE DE RMFONC ## n'apparait pas dans table ERF
xtabs(~ patri + rmfonc)
menage04$rmfonc[menage04$rmfonc== ""] <- 0
table(menage04$rmfonc, useNA="ifany")
menage04$patri[menage04$patri== ""] <- 0
table(menage04$patri, useNA="ifany")
detach(menage04)
attach(menage04)
xtabs(~patri + rmfonc)
## problème: comment traiter les 98_99 ET les 8500 NA ou 0 de rmfonc ##

## ETUDE DE RMINTER (revenus mensuels activité ou remplacement- 12 derniers mois)
table(rminter)
xtabs(~ patri + rminter)

# regression logistique
attach(menage04)
rminter[menage04$rminter==""] <- NA
ddistrminter <- datadist(rminter)
options(datadist='ddistrminter')
logitrev <- lrm(patri ~ rminter)
print(logitrev)
summary(logitrev)

newdata3 <-data.frame(rminter)
newdata3$predicted<-predict( logitrev,newdata=newdata3,type="fitted.ind")
newdata3

## ETUDE DE DIPLOPR et DIPLOCJ

## using the debin function## 
cats = c('01', '02','03','04','05','06','07','08','09','10','11','12')
bins = c( 0,3000,7500,15000,30000,45000,75000,105000,150000,225000,300000,450000,5000000)
## linéarisation du patrimoine ## 
menage04$patri[menage04$patri %in% c('','98','99')] <- NA
menage04$pat_new <- debin(menage04$patri, categories = cats, bins = bins )
View(menage04$pat_new)
## PLOT-trouver mieux ##
attach(menage04)
xtabs(~pat_new + diplopr)
table(diplopr)
ddistdiplopr <- datadist(diplopr)
options(datadist='ddistdiplopr')
logitdiplo <- lrm(patri ~ diplopr)
print(logitdiplo)
summary(logitdiplo)

logitdiplo.table <- xtable(summary(logitdiplo))
print(logitdiplo.table,floating=FALSE)

## sans 98_99
diplopr <- (c(0,1,2,3,4,5,6,7))
new_data1 <-data.frame(diplopr)
new_data1$predicted<-predict(logitdiplo,newdata=new_data1,type="fitted.ind")
new_data1
new_data1.table <- xtable(new_data1$predicted)
print(new_data1.table,floating=TRUE)


View(diplocj)
menage04$diplocj[menage04$diplocj==""] <- NA
attach(menage04)
table(diplocj)
ddistdi <- datadist(diplocj)
options(datadist='ddistdi')
logitdi <- lrm(patri~diplocj)
print(logitdi)
summary(logitdi)
diplocj <- (c(0,1,2,3,4,5,6,7))
new_data2 <-data.frame(diplocj)
new_data2$predicted<-predict(logitdi,newdata=new_data2,type="fitted.ind")
new_data2
new_data2.table <- xtable(new_data2$predicted)
print(new_data2.table,floating=TRUE)

## ETUDE DE LOGOC (statut d'occupation du logement)
attach(menage04)
View(menage04$logoc)
table(menage04$logoc, useNA="ifany")
ddlogoc <- datadist(logoc)
options(datadist='ddlogoc')
logitlogoc <- lrm(patri~logoc)
print(logitlogoc)
summary(logitlogoc)


logoc <- (c(1,2,3,4,5,6))
newdatalog <-data.frame(logoc)
newdatalog$predicted<-predict(logitlogoc,newdata=newdatalog,type="fitted.ind")
newdatalog


## ETUDE DE OCCUPPR et OCCUPCJ ## 
attach(menage04)
table(menage04$occuppr, useNA="ifany")
table(menage04$occupcj, useNA="ifany")
menage04$occupcj[menage04$occupcj==""] <- NA
ddoccup <- datadist(occuppr, occupcj)
options(datadist='ddoccup')
logitoccup<- lrm(patri~occuppr+occupcj)
print(logitoccup)
summary(logitoccup)

ddmer <- datadist(occupcj)
options(datadist='ddmer')
ddper <- datadist(occupcj)
options(datadist='ddper')
gitoccupcj <-  lrm(patri~ occupcj)
gitoccupr <- lrm(patri~ occuppr)
print(gitoccupcj)
print(gitoccupr)


## reg sur pays de naissance## 
View(individu04$paysnai)
table(individu04$paysnai, useNA="ifany")
ddpays <- datadist(individu04$paysnai)
options(datadist='ddpays')
length(individu04$paysnai)
length(menage04$patri)
logitpays <- lrm(menage04$patri~individu04$paysnai)
print(logitpays)
summary(logitpays)

logoc <- (c(1,2,3,4,5,6))
newdatalog <-data.frame(logoc)
newdatalog$predicted<-predict(logitlogoc,newdata=newdatalog,type="fitted.ind")
newdatalog

## REGRESSION LINEAIRE DE PATRI SUR VARIABLES ##
## LINEARISATION DU PATRIMOINE

cats = c('01', '02','03','04','05','06','07','08','09','10','11','12')
bins = c( 0,3000,7500,15000,30000,45000,75000,105000,150000,225000,300000,450000,5000000)

menage04$patri[menage04$patri %in% c('','98','99')] <- NA

table(menage04$patri, useNA = 'ifany')
menage04$pat_new <- debin(menage04$patri, categories = cats, bins = bins )

View(menage04[,c('pat_new', 'patri')])


## SEXE- AGE- DIPLOPR - DIPLOCJ - OCCUPPR- OCCUPCJ - LOGOC- 

detach(menage04)
attach(menage04)

## reg de patri sur logoc## ## test de significativité en cas de linéarisation du patri
reglogoc <- lm(menage04$pat_new~ logoc)
summary(reglogoc)
## LOGOC non significant- 

## travail sur deux tables différentes- ind et menage ##
df_men <- subset(menage04, select=c('ident','patri'))
df_ind <- subset(individu04, lien==1   , select=c('ident','paysnai'))
df <- merge(df_men,df_ind)
summary(df)
attach(df)
dd12<- datadist(paysnai, patri)
logleg <- lm(patri~ paysnai, data= df)
logleg
logleg.table <- xtable(logleg)
logleg.table <- xtable(logleg) ## pas moyen d'obtenir le tableau de reg en lrm
print(logleg.table, floating= TRUE)

options(datadist='dd12')
print(log12)
summary(log12)

paysnai <- (c(1,2,3,4,5,6))
newdatalog12 <-data.frame(paysnai)
newdatalog12$predicted<-predict(log12,newdata=newdatalog12,type="fitted.ind")
newdatalog12
newdatalog12.table <- xtable(newdatalog12$predicted)
print(newdatalog12.table, floating= TRUE)
## interesting finding- interprétation- études ## 

## variables croisées- faire croisé RMINTER ##

## CONSTRUIRE LE VECTEUR DANS BASE ERF#


## travaille sur épalo ##

library(rms)
df_ind1 <- subset(individu04, select=c('ident', 'epalo'))  
df_men <- subset(menage04, select=c('ident','patri'))
df1 <- merge(df_men, df_ind1)
summary(df1)
attach(df1)
dd15<- datadist(epalo, patri)
log15 <- lrm(patri~ epalo, data= df1)
options(datadist='dd15')
print(log15)
summary(log15)

epalo <- (c(1,2))
newdatalog15 <-data.frame(epalo)
newdatalog15$predicted<-predict(log15,newdata=newdatalog15,type="fitted.ind")
newdatalog15


his1 <- hist(pat$rminter, xlim=c(0,15), breaks=10)
his2 <- hist(erf$rminter, xlim=c(0,15), breaks=10)
mat <- rbind(his1$counts, his2$counts)
barplot(mat, beside = TRUE, col = c("red", "blue"), cex.names = 0.5, las = 2)

pat$rminter <- as.numeric(pat$rminter)
hist(pat$rminter, breaks=10, xlim=c(0,15), col="lightblue", xlab = "tranches de revenus", main = " enquête patrimoine")
temp <- pat[!is.na(pat$rminter), c('rminter', 'pond')]
dens <- density(temp$rminter, weights = temp$pond, na.rm = TRUE)
par(new=TRUE)
plot(dens, xlab= "", ylab="",yaxt = "n",xaxt = "n" , main="")

## using ggplot
m <- ggplot(pat ,aes(x=rminter))  
m + geom_histogram(binwidth=0.5, aes(fill = ..count.., y=..density..)) + geom_density() + scale_fill_gradient("Count", low = "green", high = "red")
 qplot(rminter, data=pat, weight=pond, geom="histogram", binwidth=1) 

m <- ggplot(pat ,aes(x=rminter))



erf$rminter <- as.numeric(erf$rminter)
hist(erf$rminter, breaks=10, xlim=c(0,15), col="lightblue", xlab = "tranches de revenus", main = " enquête ERF")
temp2 <- erf[!is.na(erf$rminter), c('rminter', 'wprm')]
dens <- density(temp2$rminter, weights = temp2$wprm, na.rm = TRUE)
par(new=TRUE)
plot(dens, xlab= "", ylab="",yaxt = "n",xaxt = "n" , main="")
View(erf)

## comparaison des salaires dans les deux enquêtes ## 

pat$rminter[pat$rminter %in% c('','98','99')] <- NA
pat$rminter <- as.numeric(pat$rminter)
describe(pat$rminter)
describe(erf$rminter)
hist(pat$rminter, xlim=c(0,15), breaks=10)
erf$rminter <- as.numeric(erf$rminter)
hist(erf$rminter, xlim=c(0,15), breaks=10)

## problème de freq- l'essentiel de la distribution rminter pour patri est répartie sur les tranches supérieures##
## sur-représentation des hauts-rev dans la table patri ## 


# table(menage04$rminter)
# a <- mean(menage04$rminter, na.rm= TRUE)
# a <- median(menage04$rminter, na.rm= TRUE)
# plot(menage06$rev_men_erf_cat)
# menage04$rminter <- as.numeric(menage04$rminter)
# a <- mean(menage06$rev_men_erf_cat, na.rm= TRUE)
# a
# a <- median(menage06$rev_men_erf_cat, na.rm= TRUE)


## etude de typmen5 dans erf, et typmen dans patri
attach(menage04)
table(menage04$typmen, useNA="ifany")
ddmen <- datadist(typmen)
options(datadist='ddmen')
logittypmen <- lrm(patri~typmen)
print(logittypmen)
summary(logittypmen)
typmen.table <- xtable(summary(logittypmen))
print(typmen.table,floating=TRUE)

typmen <- (c(1,2,3,4,5,6,7))
newdatalogmen <-data.frame(typmen)
newdatalogmen$predicted<-predict(logittypmen,newdata=newdatalogmen,type="fitted.ind")
newdatalogmen
typmen.table <- xtable(newdatalogmen$predicted)
print(typmen.table,floating=TRUE)

## predicted probas- odd ratios très intéressants- 

