rm(list = ls())

RVersion =R.Version()
if (RVersion$os != "linux-gnu") {
  setwd("C:/Users/Utilisateur/Dropbox/python_project/mSim/R/patrimoine")
} else {
  setwd("/home/benjello/Dropbox/python_project/mSim/R/patrimoine")}
rm(RVersion)

source('00_config.R')

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


## REGRESSION DE PATRI sur ##

# rminter et rmfonc (12 derniers mois)
# rmtra (revenus mensuels)
# sexepr, sexecj
# agecj, agepr
# typmen ou typfam(plus complet car inclut inactivité ou non du conjoint)
# diplopr, diplocj 

str(menage04$patri)
r <- lm(menage04$patri~ agepr + agecj + diplopr + diplocj + sexepr + sexecj + typfam +rminter +rmfonc)
summary(r)
a <- lm(menage04$patri ~ menage04$diplocj)
summary(a)
str(menage04$diplocj)
table(menage04$diplocj, useNA="ifany")

menage04$diplocj[menage04$diplocj== ""] <- NA
## compter le nombre de 98_99 dans les vecteurs##
a <- table(menage04$patri)
a[names(a) == NA]
as.data.frame(a)

b<- table(menage04$aah)
as.data.frame(b)
b[names(b)]
menage04$rmfonc[menage04$rmfonc== ""] <- 0
table(menage04$rmfonc, useNA="ifany")
str(menage04$rmfonc)
load(PatMenFil)

# Variables non retenues
## ETUDE DE AGECJ 
str(menage04$agecj)
summary(menage04$agecj)
## ETUDE DE RMTRA (revenu mensuel moyen du menage)
str(menage04$rmtra)
summary(menage04$rmtra)
table(menage04$rmtra, useNA="ifany")
describe(menage04$rmtra)
View(menage04$rmtra) 
## missing 9431! ##
## ETUDE DE RMINTER## 
View(menage04$rminter)
table(menage04$rminter, useNA="ifany")

## generate a func for log-patrimoine ##

menage04$patri[menage04$patri== ""] <- NA
table(menage04$patri, useNA="ifany")

#ETUDE SUR DEBIN ## 
# library(rJava)
# library(Deducer)
# menage04$patri <- recode.variables(menage04$patri, "01 -> runif(1, min=0, max=3000);")
# menage04$patri <- recode.variables(menage04$patri[c("01")], 01 -> runif(1, min=0, max=3000))
# menage04$patri[is.01(menage04$patri)] <- runif(1, min=0, max=3000)
# for(i in menage04$patri) if(menage04$patri %in% "01") menage04$patri <- runif(1, min=0, max=3000)
# menage04$patrinum <- replace(menage04$patri, list(1,2,3,4,5,6,7,8,9,10,11,12,98,99), runif(1, min=0, max=3000))

cats = c('01', '02','03','04','05','06','07','08','09','10','11','12')
bins = c( 0,3000,7500,15000,30000,45000,75000,105000,150000,225000,300000,450000,5000000)
pat <- subset(menage04, select=c(sexepr,agepr,occuppr,occupcj,diplopr,patri,rminter, typmen, pond))
rm(menage04)

## Débinage du patrimoine ## 
pat$patri[pat$patri %in% c('','98','99')] <- NA
table(pat$patri, useNA = 'ifany')
pat$pat_new <- debin(pat$patri, categories = cats, bins = bins )

## PATRIMOINE HISTOGRAMME ET DENSITE ## 
## REFAIRE ##

hist(pat$pat_new, breaks=50, col="lightblue", xlab = "valeurs de patrimoine", main = "histogramme et densité du patrimoine débiné")
temp <- pat[!is.na(pat$pat_new), c('pat_new', 'pond')]
temp <- temp[temp$pat_new<=500000,]
dens <- density(temp$pat_new, weights = temp$pond, na.rm = TRUE)
par(new=TRUE)
plot(dens, xlab= "", ylab="",yaxt = "n",xaxt = "n" , main="")

temp <- pat[!is.na(pat$pat_new), c('pat_new', 'pond')]
temp <- temp[temp$pat_new<=500000,]
dens <- density(temp$pat_new, weights = temp$pond, na.rm = TRUE)
plot(dens, xlab= "", ylab="", main="densité pondérée et corrigée de la variable patrimoine")


## densité de patrimoine ##  une densité non normale
describe(menage04$patri)
plot(menage04$agepr,menage04$pat_new, log='y')

## inclure l'age au carré ##
library(plyr)

menage04$agesq = menage04$agepr**2
menage04$logpat = log(menage04$pat_new)

coef1 <- lm(logpat~ agepr, data=menage04)
summary(coef1)
ress <- ddply(menage04, .(agepr), summarise, pat_mean = wtd.mean(logpat, weights=  pond, na.rm = TRUE), pat_std = sqrt(wtd.var(logpat, weights=  pond, na.rm = TRUE)))
ress$pat_bar <- predict.lm(coef1, ress)
plot(ress$agepr, ress$pat_mean)
lines(ress$agepr, ress$pat_bar)

## inclure l'age au carré## 
coef <- lm(logpat ~ agepr + agesq, data = menage04)
summary(coef)
res <- ddply(menage04, .(agepr), summarise, pat_mean = wtd.mean(logpat, weights=  pond, na.rm = TRUE), pat_std = sqrt(wtd.var(logpat, weights=  pond, na.rm = TRUE)))

res$agesq = res$agepr**2
res$pat_bar <- predict.lm(coef, res)

plot(res$agepr, res$pat_mean)
lines(res$agepr, res$pat_bar)

## plotting ##
## HOTDECK SUR LES RESIDUS ## PLOTTING
pat$patri_predicted <- predict(model, newdata=pat)
pat$resi_patri <- pat$pat_new - pat$patri_predicted
plot( pat$patri_predicted,pat$resi_patri)

pat$patri_predictedlog <- predict(modellog, newdata=pat)
residus <- pat$logpat_new - pat$patri_predictedlog
plot(pat$patri_predictedlog, residus)

# Plot residuals against fitted values (in most cases, these are the estimated conditional means, according to the model),
# since it is not uncommon for conditional variances to depend on conditional means, 
# especially to increase as conditional means increase.
# (This would show up as a funnel or megaphone shape to the residual plot.)

## check normality of residuals

BREUSCH-Pagan
bptest(model)
bptest(modellog)
summary(bptest(model))

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


## création d'une fct pour sortir les graphs
mouvante <- function(data, var , predictor){
  facet_formula <- as.formula(paste(predictor," ~ ."))  
  
  p <- ggplot(data = data, aes_string(x = var)) 
  p <- p + stat_density(aes(ymax = ..density..,  ymin = 0),
                        fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
  facet_formula <- as.formula(paste(predictor," ~ ."))
  print(facet_formula)
  p <- p + facet_grid(facet_formula) 
  print(p)
}

mouvante2 <- mouvante(data =erf, var="patri_predictedlog", predictor="diplopr")


# occuppr
library(ggplot2)
p <- ggplot(data = pat, aes(x = logpat_new)) 
p <- p + stat_density(aes(ymax = ..density..,  ymin = 0),
                      fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
p <- p + facet_grid(occuppr~ .) 
p

pp <- ggplot(data = erf, aes(x = patri_predictedlog)) 
pp <- pp + stat_density(aes(ymax = ..density..,  ymin = 0),
                        fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
pp <- pp + facet_grid( occuppr ~ .) 
pp

ppp <- ggplot(data = erf, aes(x = final)) 
ppp <- ppp + stat_density(aes(ymax = ..density..,  ymin = 0),
                          fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
ppp <- ppp + facet_grid( occuppr~ .)  
ppp

## typmen

p2 <- ggplot(data = pat, aes(x = logpat_new)) 
p2 <- p2 + stat_density(aes(ymax = ..density..,  ymin = 0),
                        fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
p2 <- p2 + facet_grid(typmen ~ .) 
p2

pp2 <- ggplot(data = erf, aes(x = patri_predictedlog)) 
pp2 <- pp2 + stat_density(aes(ymax = ..density..,  ymin = 0),
                          fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
pp2 <- pp2 + facet_grid(typmen ~ .) 
pp2

ppp2 <- ggplot(data = erf, aes(x = final)) 
ppp2 <- ppp2 + stat_density(aes(ymax = ..density..,  ymin = 0),
                            fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
ppp2 <- ppp2 + facet_grid(typmen~ .)  
ppp2

## diplopr
b <- ggplot(data = pat, aes(x = logpat_new)) 
b <- b + stat_density(aes(ymax = ..density..,  ymin = -0),
                      fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
b <- b + facet_grid(diplopr~ .) 
b

bb <- ggplot(data = erf, aes(x = patri_predictedlog)) 
bb <- bb + stat_density(aes(ymax = ..density..,  ymin = 0),
                        fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
bb <- bb + facet_grid( diplopr~ .) 
bb


bbb <- ggplot(data = erf, aes(x = final)) 
bbb <- bbb + stat_density(aes(ymax = ..density..,  ymin = 0),
                          fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
bbb <- bbb + facet_grid(diplopr~.)  
bbb

## sexepr
b2 <- ggplot(data = pat, aes(x = logpat_new)) 
b2 <- b2 + stat_density(aes(ymax = ..density..,  ymin = 0),
                        fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
b2 <- b2 + facet_grid( sexepr ~.) 
b2

bb2 <- ggplot(data = erf, aes(x = patri_predictedlog)) 
bb2 <- bb2 + stat_density(aes(ymax = ..density..,  ymin = 0),
                          fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
bb2 <- bb2 + facet_grid( sexepr~.) 
bb2


bbb2 <- ggplot(data = erf, aes(x = final)) 
bbb2 <- bbb2 + stat_density(aes(ymax = ..density..,  ymin = 0),
                            fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
bbb2 <- bbb2 + facet_grid(sexepr~.)  
bbb2

## catégories d'âges
cats = c('01','02','03','04','05','06','07','08')
bins = c( 0,20,30,40,50,60,70,80,120)
pat$agepr2 <- categorize(pat$agepr, seuil= bins, categories = cats)
View(pat[,c('agepr', 'agepr2')])

b3 <- ggplot(data = pat, aes(x = logpat_new)) 
b3 <- b3 + stat_density(aes(ymax = ..density..,  ymin = 0),
                        fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
b3 <- b3 + facet_grid(agepr2 ~ .) 
b3

cats = c('01','02','03','04','05','06','07','08')
bins = c( 0,20,30,40,50,60,70,80,120)
erf$agepr2 <- categorize(erf$agepr, seuil= bins, categories = cats)
View(erf[,c('agepr', 'agepr2')])
~
  bb3 <- ggplot(data = erf, aes(x = patri_predictedlog)) 
bb3 <- bb3 + stat_density(aes(ymax = ..density..,  ymin = 0),
                          fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
bb3 <- bb3 + facet_grid(agepr2 ~ .) 
bb3

bb3 <- ggplot(data = erf, aes(x = final)) 
bb3 <- bb3 + stat_density(aes(ymax = ..density..,  ymin = 0),
                          fill = "grey50", colour = "grey50", geom = "ribbon", position = "identity")  
bb3 <- bb3 + facet_grid(agepr2 ~ .) 
bb3


library( tikzDevice )
tikz( 'myPlot.tex' )
plot( 1, 1, main = '\\LaTex\\ is $\\int e^{xy}$' )

# dev.off()
# 
# TestChars <- function(encoding="ISOLatin1", ...)
# {
#   pdf(encoding=encoding, ...)
#   par(pty="s")
#   plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="",
#        xaxs="i", yaxs="i")
#   title(paste("Centred chars in encoding", encoding))
#   grid(17, 17, lty=1)
#   for(i in c(32:255)) {
#     x <- i %% 16
#     y <- i %/% 16
#     points(x, y, pch=i)
#   }
#   dev.off()
# }
# open a device: pdf("figures/myfile.pdf", height=6, width=6)
# plot your R object: plot(1:10, type='l', main='boring') -- and remember that lattice and ggplot need an explicit print around plot.
# important: close your device: dev.off() to finalize the file.
# optional: inspect the pdf file
# in LaTeX, use usepackage{graphics} in the document header, use
# \includegraphics[width=0.98\textwidth]{figures/myfile} to include the figure created earlier and note that file extension is optional
# run this through pdflatex and enjoy



## ETUDE RESIDENCE PRINCIPALE

str(produit04$logna)
sbset4 <- subset(produit04,logna=='02', select= c(ident, logna))
summary(sbset4)
sbset5 <- subset(produit04, !is.na(profna), select=c(ident, profna, nature, montmax, montmin))
summary(sbset5)
describe(sbset5)

sbset2 <- subset(produit04, batlog=='1' ,select= c(ident, nature, batlog, montmax, montmin))

sink("C:/Users/Utilisateur/Desktop/rapport/var_res_princ3.txt", append=TRUE, split=TRUE)

for(i in 1:77){
                id = sbset2[i,'ident']
                df <- (produit04[produit04$ident==id, ])
                df3 <- subset(df,logna !='', select= c(ident, identprod, logna, batlog, nature, montmax, montmin))
                df4 <- subset(df,logna =='01', select= c(ident, identprod, montmax, montmin))
                df4$res_princ <- ((df4$montmin+df4$montmax)/2)
                print('res princ')
                print(df4$res_princ)
                print('tot_autres_actifs_immo')
                df5 <- subset(df, logna=='02', select= c(ident, identprod, montmax, montmin))
                df5$autres_actifs_immo <- ((df5$montmin+df5$montmax)/2)
                print(df5$autres_actifs_immo)
                df6 <- subset(df, logna=='03', select= c(ident, identprod, montmax, montmin))
                df6$autres_actifs_immo <- ((df6$montmin+df6$montmax)/2)
                print(df6$autres_actifs_immo)
                df7 <- subset(df, logna=='04', select= c(ident, identprod, montmax, montmin))
                df7$autres_actifs_immo <- ((df7$montmin+df7$montmax)/2)
                print(df7$autres_actifs_immo)
                df8 <- subset(df, logna=='05', select= c(ident, identprod, montmax, montmin))
                df8$autres_actifs_immo <- ((df8$montmin+df8$montmax)/2)
                print(df8$autres_actifs_immo)
                }

sbset <- subset(menage04, logoc== '4' ,select= c(ident, logoc))
str(sbset)
id = sbset[1,'ident']



sbset <- subset(menage04, logoc== '4' ,select= c(ident, logoc))


## sort les actifs immobiliers
cum_df3 <- data.frame()
sbsetset <- subset(produit04, logna=='01'|logna =='02'|logna =='03'|logna =='04' ,select= c(ident))


                     for(i in 1:8700){
  id = sbsetset[i,'ident']
  df <- (produit04[produit04$ident==id, ])
  df3 <- subset(df, select= c(ident, identprod, logna, batlog, nature, montmax, montmin))
  print(i)
  if (i==1){ 
           cum_df3 <- df3
             
           }
        
    else {cum_df3 <- rbind(cum_df3,df3)
}
}
  
View(cum_df3)
str(sbsetset$id)        


sbsetsettry<- subset(produit04, logna=='01', select= c(ident))
str(sbsetsettry)
for(i in 1:5753){
  id = sbsetsettry[i,'ident']
  df <- (produit04[produit04$ident==id, ])
  df3 <- subset(df, select= c(ident, identprod, logna, batlog, nature, montmax, montmin))
  print(i)
  if (i==1){ 
    cum_df3 <- df3
    
  }
  
  else {cum_df3 <- rbind(cum_df3,df3)
  }
}


View(cum_df3)
table(cum_df3$montmin, useNA='ifany')
table(cum_df3$montmax, useNA='ifany')
str(sbsetset$id)


# ??
# df4 <- subset(menage04, select= c(ident,logoc))
# dataresults <- merge(df3,df4, by='ident') 
# length(dataresults)
# View(dataresults)

## sort les actifs financiers
cum_dffin <- data.frame()
str(produit04$nature)
sbsetfin <- subset(produit04, nature=='1' , select= c(ident))
str(sbsetfin)
str(produit04)
for(i in 1:61065){
  id = sbsetset[i,'ident']
  df <- (produit04[produit04$ident==id, ])
  dffin <- subset(df, select= c(ident, identprod, nature, montmax, montmin))
  print(i)
  if (i==1){ 
    cum_dffin<- dffin
    
  }
  
  else {cum_dffin <- rbind(cum_dffin,dffin)
  }
}

View(cum_dffin)
summary(produit04$finna)     
table(produit04$finna)
describe(produit04$finna)

?xtabs
?Fbwidths.by.x
?prop.table

## ensemble des produits
View(df,[13, 20:40])
#ligne 13 parmi les produits
table(produit04$batlog, useNA='ifany')
summary(produit04)
table(menage04$logoc)
length(menage04$logoc)


