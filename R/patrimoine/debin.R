library(VGAM)

debin <- function(catvar, categories, bins){
  nas <- is.na(catvar)
  catvar2 <- catvar[!nas]  
  new <- rep(NA, length(catvar))
  new2 <- rep(NA, length(catvar2))  
  k <- 1
  for (cat in categories){
    new2[catvar2 == cat] <- runif(length(catvar2[catvar2 == cat]), min = bins[k], max = bins[k+1])
    k <- k+1
  }  
  new[!nas] <- new2
  new
}

## WORKING with pareto distribution

debin2 <- function(catvar, categories, bins){
  nas <- is.na(catvar)
  catvar2 <- catvar[!nas]  
  new <- rep(NA, length(catvar))
  new2 <- rep(NA, length(catvar2))  
  k <- 1
  for (cat in categories){
    if (cat!= 12){
      new2[catvar2 == cat] <- runif(length(catvar2[catvar2 == cat]), min = bins[k], max = bins[k+1])
      k <- k+1}
    else{
          alpha = 2
          min_par= 450000
          new2[catvar2 == cat] <- rpareto(length(catvar2[catvar2 == cat]),min_par,alpha)
         }
  }
  new[!nas] <- new2
  new
}

# sum(new2[catvar2 == 12])
#pat$pat_newz<- debin2(pat$patri, categories = cats, bins = bins)
#summary(pat$pat_newz)
# 
# pat$zzz <- ifelse((pat$pat_newz <= 2339000), 0, 1) 
# table(pat$zzz, useNA='ifany')

### check with else replace with 0 that the if/else works

### gen pareto distribution with R
# it has a relation to the uniform distribution thru 
# F(X) = U ~ uniform(0, 1), 
# where X is  Pareto with cdf F. Solve for X and you have just reinvented the
# "inverse method" of generating random numbers from a cdf F.
# rpareto <- function(n, min_par,alpha){
#   qpareto(runif(n), min_par,alpha)}

#http://astrostatistics.psu.edu/datasets/R/Random.html
# alpha = 3
# min_par= 450000
# describe(pat$patri)
# rpareto(100,min_par,alpha)
# 
# pat$pat_newz<- debin2(pat$patri, categories = cats, bins = bins)
# describe(pat$pat_newz)
# pat$zzz <- ifelse((pat$pat_newz <= 732000 ), 0, 1) 
# table(pat$zzz, useNA='ifany')

######################
categorize <- function(linvar, seuil, categories) {
  # TODO return error if length(seuil) != length(categories)
  new <- rep(NA, length(linvar))
  k <- 1
  for (cat in categories) {
    if (k != length(categories)) {
      new[(seuil[k] <= linvar) & (linvar<= seuil[k+1])]<- cat
      k = k+1 }
    else {
      new[(seuil[k] <= linvar)] <- cat }
    }
  new
}
#############

# cats = c('01', '02','03','04','05','06','07','08','09','10','11','12')
# bins = c( 0,3000,7500,15000,30000,45000,75000,105000,150000,225000,300000,450000,5000000)
# 
# menage04$patri[menage04$patri %in% c('','98','99')] <- NA
# 
# table(menage04$patri, useNA = 'ifany')
# menage04$pat_new <- debin(menage04$patri, categories = cats, bins = bins )
# 
# 
# cats = c('01', '02','03','04','05','06','07','08','09','10','11','12')
# seuils = c( 0,3000,7500,15000,30000,45000,75000,105000,150000,225000,300000,450000,5000000)
#                                               
# menage04$patri2 <- categorize(menage04$pat_new, seuil=bins, categories=cats )
# 
# 
# table(menage04$patri)
# table(menage04$patri2)
# 
# menage04<-menage04[,!(names(menage04) %in% c('patri2'))]
# 
# View(menage04[,c('pat_new', 'patri', 'patri2')])
# 
