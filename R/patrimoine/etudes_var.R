## IMPUTATION DU PATRIMOINE ##
rm(list = ls())
RVersion =R.Version()
if (RVersion$os != "linux-gnu") {
  setwd("C:/Users/Utilisateur/Dropbox/python_project/mSim/R/patrimoine")
} else {
  setwd("/home/benjello/Dropbox/python_project/mSim/R/patrimoine")}
rm(RVersion)

library(Hmisc)
library(reshape)
library(StatMatch)
library(ggplot2)


source('00_config.R')
source('debin.R')


## Preparing patrimoine dataframe for imputation

load(patMenFil)
## repérer si les NAs dans OCCUPCJ sont sont ceux des célibs ## 
# menage04$viecoucj <- as.numeric(menage04$viecoucj)
# menage04$viecoupr <- as.numeric(menage04$viecoupr)
# menage04$viecoup <- menage04$viecoucj + menage04$viecoupr
# table(menage04$viecoup, useNA='ifany')
# table(menage04$occupcj)
# names(menage04)
# describe(menage04$pond)
# sum(menage04$pond)
# hist(menage04$pond, breaks=20)
# load(menm)
# hist(menagem$wprm, breaks=100)
# sum(menagem$wprm)

# build patrimoine dataframe

cats = c('01', '02','03','04','05','06','07','08','09','10','11','12')
bins = c( 0,3000,7500,15000,30000,45000,75000,105000,150000,225000,300000,450000,5000000)
pat <- subset(menage04, select=c(sexepr,agepr,occuppr,occupcj,diplopr,patri,rminter, typmen, pond))
rm(menage04)

## linéarisation du patrimoine ## 
pat$patri[pat$patri %in% c('','98','99')] <- NA
table(pat$patri, useNA = 'ifany')
pat$pat_new <- debin(pat$patri, categories = cats, bins = bins )

## log
pat$logpat_new <- log(pat$pat_new)

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

## change les hauts patrimoines en 12 pr matching avec erf
pat$rminter[pat$rminter=='98'] <- NA
pat$rminter[pat$rminter=='99'] <- NA
pat$rminter[pat$rminter==''] <- NA
# pat$rminter[pat$rminter=='13'] <- '12'

cats = c('01', '02','03','04','05','06','07','08','09','10','11','12','13')
seuils = c( 0,2500,5000,7500,95000,12000, 14500, 20000, 25000, 30000, 36000, 48000, 72000, 215600)
pat$rminterdeb<- debin(pat$rminter, categories = cats, bins = seuils)
pat$logrminterdeb <- log(pat$rminterdeb)

# table(pat$rminter, useNA='ifany')
# pat$rminter <-as.factor(as.character(pat$rminter))

describe(pat$rminter)
str(pat$rminter)
pat$occuppr <- as.factor(pat$occuppr)
pat$occupcj <- as.factor(pat$occupcj)
pat$sexepr <- as.factor(pat$sexepr)
pat$diplopr <- as.factor(pat$diplopr)
pat$typmen <- as.factor(pat$typmen)

#pat$rminter <-as.factor(as.ordered(as.numeric(pat$rminter)))



library(rms)
## regression sur les variables significatives
regtot <- lm(pat$pat_new~ sexepr + agepr + diplopr+ occuppr + occupcj + rminter)
summary(regtot)
## non-inclusion de sexecj car risque de sur-représentation du modèle- abscence de diplocj  dans ERF ##
## Rsquared de 0.20##


## Preparing erf table
load(menm)

menagem$celib[menagem$typmen5 == 1] <- 1
menagem$celib[menagem$typmen5 == 2] <- 1
menagem$celib[menagem$typmen5 != 1] <- 0
table(menagem$celib, useNA='ifany')
table(menagem$acteu5cj, useNA='ifany')
length(menagem$acteu5cj)
table(menagem$typmen5)
length(menagem$celib)

# building subset from erf to impute patrimoine
erf <- subset(menagem, select= c(spr, agepr, ddipl, acteu5pr, acteu5cj, ztsam, zperm, typmen5, wprm))
rm(menagem)
# build categories of rev_men
erf$rev_men <- erf$ztsam+ erf$zperm
erf$ztsam <- NULL
erf$zperm <- NULL
cats = c('01', '02','03','04','05','06','07','08','09','10','11','12','13')
seuils = c( 0,2500,5000,7500,95000,12000, 14500, 20000, 25000, 30000, 36000, 48000, 72000)
length(cats)
length(seuils)
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

# # erf$rev_men_cat <- categorize(erf$rev_men, seuil=seuils, categories=cats )
# # erf$rev_men <- NULL
# table(erf$rev_men_cat)
# table(erf$rev_men_cat, useNA="ifany")
# summary(erf$rev_men_cat)
# describe(erf$rev_men_cat)

# TODO: within

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

## pas mal du tout en terme de pourcentage de type de pop sur les cat de diplomes##


## Pour effectuer le predict - il nous faut les mêmes noms de variables dans le newdata ##
## on renomme les vr 
erf$occuppr <- erf$acteu5pr
erf$occupcj <- erf$acteu5cj
erf$sexepr <- erf$spr
erf$diplopr <- erf$ddipl
# erf$rminter <-erf$rev_men_cat
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

# TODO: should it be ordered ?
# erf$rminter <-as.factor(as.ordered(as.numeric(erf$rminter)))




#model <- lm(pat_new~ sexepr + agepr + I(agepr**2) + diplopr + occuppr + occupcj + rminter + typmen , data=pat)

modellog <-lm(logpat_new ~ sexepr + agepr +I(agepr**2) + diplopr + occuppr + occupcj + logrminterdeb + typmen, data=pat)
            
#model2 <- lm(pat_new~rminter, data=pat)
## modelbis <- lm(pat_new ~ sexepr + agepr + diplopr + occuppr + rminter, data=pat)
summary(model2)

#summary(model)
summary(modellog)
summary(modellog)$sigma
## summary(modelbis)
# test <- na.omit(erf)
# test <- erf
# names(test)
# describe(test)


erf$patri_predictedlog <- predict(modellog, newdata=erf)

erf$patri_predicted <- predict(model, newdata=erf)
## erf$patri_predicted <- predict(modelbis, newdata=erf)

## HOTDECK SUR LES RESIDUS
pat$patri_predicted <- predict(model, newdata=pat)
pat$resi_patri <- pat$pat_new - pat$patri_predicted

## en log
pat$patri_predicted <- predict(modellog, newdata=pat)
pat$resi_patri <- pat$logpat_new - pat$patri_predicted

## check normality of residuals

## double-check all this- rediger!
## comparaison des distributions
## avec et sans log- quels sont les histogrammes?
## + normalité des res

View(pat)
hist(pat$resi_patri, breaks=100)
qqnorm(pat$resi_patri)
qqline(pat$resi_patri)


plot(pat$agepr, pat$pat_new, type="h")
hist(pat$pat_new, breaks= 100, plot=TRUE)
hist(pat$patri_predicted, breaks=100, plot=TRUE)
par(new=TRUE)
plot(pat$agepr, pat$patri_predicted, type="h",col='red')

## en log
plot(pat$agepr, pat$logpat_new, type="h")
par(new=TRUE)
plot(pat$agepr, pat$patri_predicted, type="h",col='red')
hist(pat$logpat_new, breaks= 100, plot=TRUE)
hist(pat$patri_predicted, breaks=100, plot=TRUE)

#donation classes

str(erf$occuppr)
pat <- na.omit(pat)

# pat$rminter2 <- as.factor(ifelse(pat$rminter=='13','12',pat$rminter))
# erf$rminter2 <- as.factor(ifelse(erf$rminter=='13','12',erf$rminter))

allvars <- c("occuppr", "agepr","occupcj","diplopr","rminter2", 'sexepr', 'typmen')
classes <- c("occuppr", "typmen") 
#matching variables
matchvars <- setdiff(allvars, classes)
gc()
out.nnd <- NND.hotdeck(data.rec=erf,data.don= pat,match.vars=matchvars, don.class= classes, gdist.fun="Gower")

erf <- create.fused(data.rec=erf, data.don=pat ,mtc.ids=out.nnd$mtc.ids, z.vars="resi_patri")
rm(matchvars)
str(pat)
erf$final <- erf$patri_predicted + erf$resi_patri
View(erf)
plot(erf$agepr, erf$final, type='h', col='red')
par(new=TRUE)
plot( pat$agepr, pat$pat_new, type='h' )
library(lmtest)
w <- bptest(model, data=pat)
summary(w)


## en LOG 
## 1ere stratégie
pat$residuslog1 <- pat$logpat_new - pat$patri_predicted
## ei = logPi - logPipredi
 ## hotdeck
allvars <- c("occuppr", "agepr","occupcj","diplopr","logrminterdeb", 'sexepr', 'typmen')
classes <- c("occuppr", "typmen") 
#matching variables
matchvars <- setdiff(allvars, classes)
gc()
out.nnd <- NND.hotdeck(data.rec=erf,data.don= pat,match.vars=matchvars, don.class= classes, gdist.fun="Gower")

erf <- create.fused(data.rec=erf, data.don=pat ,mtc.ids=out.nnd$mtc.ids, z.vars="residuslog1")
rm(matchvars)
str(pat)
erf$final <- erf$patri_predicted + erf$residuslog1
## ET PASSAGE EN EXPONENTIELLE AVOIR LA VALEUR DU PATRIMOINE?
erf$finalexp <- exp(erf$final)
View(erf)

## CUMULATIVE DISTRIB FCT
## 2nd stratégie
pat$patri_predicted <- predict(modellog, newdata=pat)
residus <- pat$logpat_new - pat$patri_predicted
pat$logpat_predi_corrige <- exp(pat$patri-predicted +(sd(residus)**2)/2)
residuslog2 <- pat$pat_new - pat$logpat_predi_corrige
sd((residus)**2)/2


hist(pat$pat_new, breaks=100)
cr.plots(modellog)
plot(modellog)
## lir doc ##


##hotdeck- attribution des résidus à exp(p_erf)
erf$patri_predictedlog <- predict(modellog, newdata=erf)

allvars <- c("occuppr", "agepr","occupcj","diplopr","logrminter", 'sexepr', 'typmen')
classes <- c("occuppr", "typmen") 
#matching variables
matchvars <- setdiff(allvars, classes)
gc()
out.nnd <- NND.hotdeck(data.rec=erf,data.don= pat,match.vars=matchvars, don.class= classes, gdist.fun="Gower")

erf <- create.fused(data.rec=erf, data.don=pat ,mtc.ids=out.nnd$mtc.ids, z.vars="residuslog2")
rm(matchvars)
erf$patri_predicted <- log(erf$patri_predicted)
## check ## 
erf$final <- erf$patri_predicted + erf$residuslog2
View(erf)




















library(ggplot2)

p <- ggplot(data = pat, aes(x = pat_new)) 
p <- p + stat_density(aes(ymax = ..density..,  ymin = -..density..),
               fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
p <- p + facet_grid(. ~ occuppr) +   coord_flip() 
p
?as.ordered
## linéariser avant/apres log
## log/ cvx/ check (distributions)
## 


## CDF of rents res
erfCDF  <- wtd.Ecdf(erf$patri_predictedlog,weights=erf$wprm);
patCDF <- wtd.Ecdf(pat$patri_predicted,weights=pat$pond);
erfCDF_data <- data.frame(x=erfCDF$x,cdf=erfCDF$ecdf,survey="erf")
patCDF_data <- data.frame(x=patCDF$x,cdf=patCDF$ecdf,survey="pat")
plot_data <- data.frame(rbind(erfCDF_data,patCDF_data))
ggplot(plot_data, aes(x=x,y=cdf, color=survey)) + geom_point()

describe(res_fill.erf.nnd$lmlm,weights=res_fill.erf.nnd$wprm)
describe(logt$lmlm,weights=logt$wprm)






## generate a regressor
x <- rep(c(-1,1), 50)
## generate heteroskedastic and homoskedastic disturbances
err1 <- rnorm(100, sd=rep(c(1,2), 50))
err2 <- rnorm(100)
## generate a linear relationship
y1 <- 1 + x + err1
y2 <- 1 + x + err2
## perform Breusch-Pagan test
bptest(y1 ~ x)
bptest(y2 ~ x)


