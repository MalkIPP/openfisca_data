## More on hotdeck:
## finding the best classes among all vars

## 1) you need to define a model (for hotdeck)
## 2) define all vars
## 3) define impute_x function 
## 4) checking than impute works as standard hotdeck (below)
## 5) define compare_results function (compare les distributions initiale et imputé)
## 6) for different 'classes vars' use compare_results function on the initial set: 
## cut in half the set (A and B)
## impute value from A in set B
## compare B to initial set


## define function impute_patri d'imputation du patrimoine
impute_patri <- function(imputed, ref, allvars, classes){
  require(car)
  require(MASS)  
  # use formula
  modellog <- lm(logpat_new ~ sexepr + agepr +I(agepr**2) + diplopr + occuppr + occupcj + logrminterdeb + typmen, data=ref)
  ref$patri_predictedlog <- predict(modellog, newdata=ref)
  imputed$patri_predictedlog<- predict(modellog, newdata=imputed)
  ref$res = ref$logpat_new-ref$patri_predictedlog
  matchvars <- setdiff(allvars, classes)
  gc()
  twice.out.nnd <- NND.hotdeck(data.rec=imputed,data.don= ref,match.vars=matchvars, don.class= classes, gdist.fun="Manhattan")
  
  imputed <- create.fused(data.rec=imputed, data.don=ref ,mtc.ids=twice.out.nnd$mtc.ids, z.vars="res")
  rm(matchvars)
  imputed$final <- imputed$patri_predictedlog + imputed$res
  ## ET PASSAGE EN EXPONENTIELLE POUR AVOIR LA VALEUR DU PATRIMOINE
  imputed$finalexp <- exp(imputed$final)
  return(imputed)
}

# s$patri_predictedlog<- predict(modellog, newdata=s)
# out.nnd <- NND.hotdeck(data.rec=s,data.don= ref,match.vars=matchvars, don.class= classes, gdist.fun="Manhattan")
# head(imputed)
# head(pat)
# colnames(erf1)
# allvars %in% names(erf1)
# erf$id <- 1:nrow(erf)
# erf1 <- subset(erf,select=c(sexepr,agepr,rev_men, occuppr,occupcj,diplopr, rminter, typmen,wprm, logrminterdeb, id))
# pat <- subset(pat, select=c(patri_predictedlog, sexepr,agepr,occuppr,occupcj,diplopr,patri,rminter, typmen, pond, logpat_new, logrminterdeb))
# erf2 <- impute_patri(erf1, pat, allvars, classes)
# erf   <- data.frame(id=as.integer(erf$id), x=erf$finalexp)
# erf2    <- data.frame(id=as.integer(erf2$id), x0=erf2$finalexp)
# errors <- merge(erf,erf2, by="id" )
# errors$rel <- errors$x-errors$x0
# describe(errors)
# checking que la fonction est bien la même que hotdeck ci-dessus:  TOP #


## define compare_results function 
compare_results  <- function(imputed, reference){
  imp    <- data.frame(id=as.integer(imputed$id), x=imputed$finalexp)
  ref    <- data.frame( x0=reference$pat_new, id=as.integer(reference$id))
  errors <- merge(imp,ref, by="id" )
  errors$rel <- errors$x-errors$x0
  errors$relpct <-  errors$rel/errors$x0
  errors$abs <- abs(errors$x-errors$x0)
  errors$abspct <-  errors$abs/errors$x0
  return(errors)

# SEED (432)
# pat$patri_predictedlog <- predict(modellog, newdata=pat)
# pat <- subset(pat, select=c(patri_predictedlog, sexepr,agepr,occuppr,occupcj,diplopr,patri,rminter, typmen, pond, logpat_new, logrminterdeb, pat_new, agepr_cat))
# set.seed(432)
# pat$id <- 1:nrow(pat)
# s <- pat[sample(nrow(pat), round(nrow(pat)/2)),]
# ref <- pat[!(pat$id %in%  unique(s$id)),]
# # unique returns a vector or df but with duplicate elements removed
# imputed <- impute_patri(s, ref, allvars, classes) 
# 
# compare_results  <- function(imputed, reference){
#   imp    <- data.frame(id=as.integer(imputed$id), x=imputed$finalexp)
#   ref    <- data.frame( x0=reference$pat_new, id=as.integer(reference$id))
#   errors <- merge(imp,ref, by="id" )
#   errors$rel <- errors$x-errors$x0
#   errors$relpct <-  errors$rel/errors$x0
#   errors$abs <- abs(errors$x-errors$x0)
#   errors$abspct <-  errors$abs/errors$x0
#   describe(errors[,c("rel","relpct","abs","abspct")])
#   
# }
# compare_results(imputed, pat)
}
####################################################################################
########### FINDING THE BEST CLASSES (don-classes)

## seq of 2 numbers in length(allvars): tirage de 2 parmi length(allvars) 
## On peut utiliser des classes qui n'apparaissent pas dans la régression: rminter et agepr_cat
## remarque: dans les allvars on garde les variables en continue: logrminter et agepr

choice_classes <- c("occuppr","diplopr", "sexepr", "typmen", "agepr_cat", "rminter")
a <- combn(choice_classes, 2, FUN = NULL, simplify = TRUE)
ncol(a)

# sink("C:/Users/Utilisateur/Desktop/rapport/test_classes3.txt", append=TRUE, split=TRUE)
# ## save all the computations in a external file
# 
# set.seed(12345)
# 
# cum_errors <- data.frame()
# 
# 
# for(i in 1:ncol(a)) {
#   classes <- (a[1:nrow(a),i])
#   print(classes)
#   for (j in 1:10) { 
#     if (j==1) {isnewclasses = TRUE}
#     pat$id <- 1:nrow(pat)
#     s <- pat[sample(nrow(pat), round(nrow(pat)/2)),]
#     ref <- pat[!(pat$id %in%  unique(s$id)),]
#     imputed <- try(impute_patri(s, ref, allvars, classes))
#     if (!(class(imputed)=="try-error")) {
#       errors <- compare_results(imputed, pat)  
#       errors$nb <- j
#       if (isnewclasses) {
#        cum_errors <- errors
#       isnewclasses <- FALSE
#       } 
#       else {
#         cum_errors <- rbind(cum_errors,errors)    # que fais-t-il pour lier sans connaitre "errors"?
#       } }
#   print(isnewclasses)
#   print(describe(cum_errors$abspct))
#   }
# }

#######################################################################################
## observations pour un cas particulier##
# sink("C:/Users/Utilisateur/Desktop/rapport/test_classes9.txt", append=TRUE, split=TRUE)
# classes= c('occupcj', 'occuppr')
# for (j in 1:10) {
# pat$id <- 1:nrow(pat)
# s <- pat[sample(nrow(pat), round(nrow(pat)/2)),]
# ref <- pat[!(pat$id %in%  unique(s$id)),]
# imputed <- try(impute_patri(s, ref, allvars, classes))
# if (!(class(imputed)=="try-error")) {
#   errors <- compare_results(imputed, pat)  
#   errors$nb <- j
# print(median(errors$abspct, na.rm=TRUE))}
# }
# ###########################################################################################""
# ## TEST ## rather than impute_patri ## step by step
# set.seed(256)
# pat$patri_predictedlog <- predict(modellog, newdata=pat)
# sum(is.na(pat$pat_new))
# sum(is.na(pat$patri_predictedlog))
# pat <- subset(pat, select=c(patri_predictedlog, sexepr,agepr,occuppr,occupcj,diplopr,patri,rminter, typmen, pond, logpat_new, logrminterdeb, pat_new))
# pat$id <- 1:nrow(pat)
# s <- pat[sample(nrow(pat), round(nrow(pat)/2)),]
# ref <- pat[!(pat$id %in%  unique(s$id)),]
# allvars <- c("occuppr", "agepr","occupcj","diplopr","logrminterdeb", "sexepr", "typmen")
# classes <- c("sexepr", "typmen") 
# #matching variables
# matchvars <- setdiff(allvars, classes)
# gc()
# 
# s$patri_predictedlog <- predict(modellog, newdata=s)
# ref$patri_predictedlog<- predict(modellog, newdata=ref)
# ref$res = ref$logpat_new-ref$patri_predictedlog
# out.nnd <- NND.hotdeck(data.rec=s,data.don= ref,match.vars=matchvars, don.class= classes, gdist.fun="Manhattan")
# 
# imputed <- create.fused(data.rec=s, data.don=ref ,mtc.ids=out.nnd$mtc.ids, z.vars="res")
# rm(matchvars)
# imputed$final <- imputed$patri_predictedlog + imputed$res
# ## ET PASSAGE EN EXPONENTIELLE POUR AVOIR LA VALEUR DU PATRIMOINE
# imputed$finalexp <- exp(imputed$final)
# 
# sum(is.na(imp$x))
# sum(is.na(ref$pat_new))
# head(errors)
# imp    <- data.frame(x=imputed$finalexp, id=as.integer(imputed$id))
# ref    <- data.frame( x0=ref$pat_new, id=as.integer(ref$id))
# errors <- merge(imp,ref, by="id" )
# errors$rel <- errors$x-errors$x0
# describe(errors[,c("rel")])






#p + geom_histogram(aes(x=cat, weight=w),binwidth=0.5) + facet_wrap(~source)


### DENSITES PONDEREES EN FONCTION DES VAR DE CLASSES

m <- ggplot(pat, aes(x=pat_new,weight=pond,colour= agepr_cat, group=agepr_cat)) + geom_density() 
m + scale_x_log10() 

m <- ggplot(pat, aes(x=finalexp,weight=pond,colour= agepr_cat, group=agepr_cat)) + geom_density()
m + scale_x_log10()
m <- ggplot(erf, aes(x=finalexp,weight=wprm,colour= agepr_cat, group=agepr_cat)) + geom_density()
m + scale_x_log10()


qplot(rminter, data=pat, weight=pond, geom="histogram", binwidth=1) 
#####
n <- ggplot(erf ,aes(x=patri_cat,weight=wprm))
n <- ggplot(erf ,aes(x=patri_cat))
n + geom_histogram(binwidth=0.5, aes(fill = ..count.., y=..density.., weight=wprm)) + geom_density() + scale_fill_gradient("Count", low = "yellow", high = "red")

erf$patri_cat <- as.numeric(erf$patri_cat)
erfpatri_cat <- as.factor(erf$patri_cat)
## get warnings and no colors for counts when adding weight in n

## linéariser avant/apres log
hist(pat$pat_new, breaks=100)
cr.plots(modellog)
cr.plots(model)
plot(modellog)
plot(model)

## OBSERVATIONS DES DISTRIBUTIONS
## CDF avec log
erfCDF  <- wtd.Ecdf(erf$patri_predictedlog,weights=erf$wprm);
patCDF <- wtd.Ecdf(pat$patri_predictedlog,weights=pat$pond);
erfCDF_data <- data.frame(x=erfCDF$x,cdf=erfCDF$ecdf,survey="erf")
patCDF_data <- data.frame(x=patCDF$x,cdf=patCDF$ecdf,survey="pat")
plot_data <- data.frame(rbind(erfCDF_data,patCDF_data))
ggplot(plot_data, aes(x=x,y=cdf, color=survey)) + geom_point()

describe(erf$patri_predictedlog,weights=erf$wprm)
describe(pat$patri_predictedlog,weights=pat$pond)

## CDF sans log
erfCDF  <- wtd.Ecdf(erf$finalexp,weights=erf$wprm);
patCDF <- wtd.Ecdf(pat$pat_new,weights=pat$pond);
patpredCDF <- wtd.Ecdf(pat$finalexp,weights=pat$pond);
erfCDF_data <- data.frame(x=erfCDF$x,cdf=erfCDF$ecdf,survey="erf")
patCDF_data <- data.frame(x=patCDF$x,cdf=patCDF$ecdf,survey="pat")
patpredCDF_data <- data.frame(x=patpredCDF$x,cdf=patpredCDF$ecdf,survey="patpred")
plot_data <- data.frame(rbind(erfCDF_data,patCDF_data,patpredCDF_data))
ggplot(plot_data, aes(x=x,y=cdf, color=survey)) + geom_point() + scale_x_log10()


describe(erf$patri_predictedlog,weights=erf$wprm)
describe(pat$patri_predictedlog,weights=pat$pond)
describe(pat$pat_new, weights=pat$pond)
str(erf)