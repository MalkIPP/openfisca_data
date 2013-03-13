## METHODO ##

BREUSCH-Pagan
bptest(model)
bptest(modellog)
summary(bptest(model))

## checking the number of NA's
sum(is.na(pat$final))
sum(is.na(pat$pat_new))
sum(is.na(pat$patri_predictedlog))
sum(is.na(pat$occuppr))
sum(is.na(pat$residuslog1))
sum(is.na(pat$finalexp))


sum(is.na(erf$residuslog1))
sum(is.na(erf$final))
sum(is.na(erf$patri_predictedlog))
sum(is.na(erf$occuppr))
sum(is.na(erf$residuslog1))


########################
describe(pat$pat_new)
describe(erf$finalexp)
describe(pat$finalexp)
#########################
erf2 <- na.omit(erf)
dens <- density(erf2$finalexp, weights = erf2$wprm, na.rm = TRUE)
erf2$finalexp[erf$finalexp=='NaN']<- 0
plot(dens, xlab= "", ylab="", main="")
describe(erf2$finalexp, weights= erf2$wprm)


## USING WEIGHTS
# table(menage04$occupcj)
# names(menage04)
# describe(menage04$pond)
# sum(menage04$pond)
# hist(menage04$pond, breaks=20)
# load(menm)
# hist(menagem$wprm, breaks=100)
# sum(menagem$wprm)


## Déterminer la nature d'une erreur
x <- try(log("a"))
class(x)=="try-error"

# utiliser if/else
# if (classes == c("sexepr", "typmen")) {return 'unappropriate classes'}
# else{ ...bug avec cette classe...parfois...

## gérer les warnings

# enlever warning sur console/ NND
options(warn=-1) 
options()
## create warning
testit <- function() warning("testit")
testit() ## shows call
testit <- function() warning("problem in testit", call. = FALSE)
testit() ## no call
## suppress?
suppressWarnings(warning("the number of donors is less than the number of recipients"))
?suppressWarnings(expr)


f <- function( x) {
  warning( "bla bla" )
  y <- x + 3
  warning( "yada yada" )
  y
}
suppressWarnings( f(5) )

## autres functions
?invokeRestart
http://stat.ethz.ch/R-manual/R-patched/library/base/html/conditions.html

## diverting errors messages to sink 
con<-file('test',open='w')
sink(con,type='message',append=TRUE)
try(log("a"))
readLines('test')

con<-file('test',open='w')
sink(con)
## Now produce some output
rnorm(10)

## produce an error message
rnorm('abc')
## more output
rnorm(10)
## stop sinking for messages and output
sink(NULL,type='message')
sink(NULL)
## see what went into the file

## COORDINATION OU/ET &
logna ='02'|logna='03'|logna='04'


## 2nd stratégie REJETE POUR IMPUTATION RESIDUS
# pat$patri_predictedlog <- predict(modellog, newdata=pat)
# residus <- pat$logpat_new - pat$patri_predictedlog
# sig <- summary(model)$sigma
# pat$logpat_predi_corrige <- exp(pat$patri-predictedlog + sig**2/2)
# residuslog2 <- pat$pat_new - pat$logpat_predi_corrige
# 
# ##hotdeck- attribution des résidus à exp(p_erf)
# erf$patri_predictedlog <- predict(modellog, newdata=erf)
# erf$patri_predicted <- exp(erf$patri_predictedlog)
# allvars <- c("occuppr", "agepr","occupcj","diplopr","logrminter", 'sexepr', 'typmen')
# classes <- c("occuppr", "typmen") 
# #matching variables
# matchvars <- setdiff(allvars, classes)
# gc()
# out.nnd <- NND.hotdeck(data.rec=erf,data.don= pat,match.vars=matchvars, don.class= classes, gdist.fun="Gower")
# 
# erf <- create.fused(data.rec=erf, data.don=pat ,mtc.ids=out.nnd$mtc.ids, z.vars="residuslog2")
# rm(matchvars)
# erf$final <- erf$patri_predicted + erf$residuslog2
# View(erf)



### observer les na
## verifier consistence des modèles
summary(model)
summary(modellog)

sum(is.na(pat$pat_new))
sum(is.na(pat$patri_predictedlog))
sum(is.na(pat$occuppr))
sum(is.na(pat$residuslog1))


## tentative de sortir des tableaux de type describe
library(plyr) 
library(xtable)
patri_cat<- sapply(erf$patri_cat, describe)
patri_cat.table <- xtable(erf$patri_cat)
print(patri_cat.table,floating=TRUE)
## 
describe(erf$rminter)
m <- ggplot(pat ,aes(x=rminter))  
m + geom_histogram(binwidth=0.5, aes(fill = ..count.., y=..density..)) + geom_density() + scale_fill_gradient("Count", low = "green", high = "red")
#######
qplot(rminter, data=pat, weight=pond, geom="histogram", binwidth=1) 
#####
m <- ggplot(erf ,aes(x=rminter))
m + geom_histogram(binwidth=0.5, aes(fill = ..count.., y=..density..)) + geom_density() + scale_fill_gradient("Count", low = "green", high = "red")


n <- ggplot(pat)
n + geom_histogram(aes(x=patri, weight=pond),binwidth=0.5)

n <- ggplot(erf)
n + geom_histogram(aes(x=patri_cat, weight=wprm),binwidth=0.5)

x = pat$pond                   
sum(x[ pat$patri=="11" & !is.na(pat$patri)])


## COMPARER LES HISTOGRAMMES ET DENSITES UNE FOIS LES VAR DE CLASSES TROUVES PR OPTIMISATION 
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
plot(pat$agepr, pat$patri_predictedlog, type="h",col='red')
hist(pat$logpat_new, breaks= 100, plot=TRUE)
hist(pat$patri_predictedlog, breaks=100, plot=TRUE)



## construction d'un sous-ensemble pour étudier déterminant biens prof
# profn <- subset(pat, select=c(ident,sexepr,agepr,occuppr,occupcj,diplopr,rminter, typmen, tu99, pond))
# profn1 <- subset (produit04,!is.na(profna), select=c(ident,identpos,identprod,profna, pond, nature, montmax, montmin))
# profn2 <- merge(profn1, profn, by="ident")
# profn2$profna <- as.numeric(profn2$profna)
# str(profn2$profna)
# profn2$profna[!is.na(profn2$profna)] <- 1
# profn2$profna[is.na(profn2$profna)] <- 0
# table(profn2$profna)

# ## on définit une dummy 'possède des biens professionnels'
# ## on voudrait imputer cette dummy dans ERF
# table(profn2$profna, useNA='ifany')
# 
# profnn <- subset(profn2, !is.na(profna))
# names(profnn)
# # describe(profnn)
# table(diplopr, useNA='ifany')
## reg d'une variable binaire sur des variables ordinales
# attach(profnn)
# profnn$profna <- as.factor(profnn$profna)
# regprof <- glm(profna ~ sexepr + agepr + I(agepr**2) + diplopr + occuppr + occupcj + rminter + typmen + tu99, data=profnn, family= binomial)
# summary(regprof)
# profnn$profna2 <- predict(regprof, newdata=profnn, type="response")
# summary(profnn$profna2)
# View(profnn[,c('profna2', 'profna')])
# detach(profnn)

## dans base ERF prédire 
# erf$profna <- predict(regprof, newdata=erf, type="response")
# summary(erf$profna, useNA='ifany')

