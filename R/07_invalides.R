# OpenFisca


##***********************************************************************/
message('07_invalides: construction de la variable invalide')
##***********************************************************************/


# Invalides
#inv = caseP (vous), caseF (conj) ou case G, caseI, ou caseR (pac)

loadTmp("final.Rdata")
invalides <- final[,c("noindiv","idmen","caseP","caseF","idfoy","quifoy")]
invalides <- within(invalides,{
  caseP <- ifelse(is.na(caseP),0,caseP)
  caseF <- ifelse(is.na(caseF),0,caseF)
  inv <- FALSE})

# Les "vous" invalides
table(invalides[,c("caseF","quifoy")],useNA="ifany")
invalides[(invalides$caseP==1) & (invalides$quifoy=="vous"),"inv"] <- TRUE

# Les conjoints invalides

#men_inv_conj <- invalides[c("idmen","caseF","quifoy")] 
#men_inv_conj <- rename(men_inv_conj, c("caseF"="inv"))
#table(men_inv_conj[men_inv_conj$inv==1 ,c("inv","quifoy")],useNA="ifany")
# Il y a des caseF suir des conjoints cela vint des doubles d?clarations TODO shoumd clean this
#toto <- invalides[invalides$caseF==1 & invalides$quifoy=="conj","idmen"]
#load(indm)
#titi <- indivim[(indivim$ident %in% toto) & (indivim$persfip=="vous" |indivim$persfip=="conj") ,c("ident","noindiv","declar1","declar2","persfip","quelfic")]
#titi <- titi[order(titi$ident),]
foy_inv_conj <- invalides[,c("idfoy","caseF","quifoy")] 
foy_inv_conj <- rename(foy_inv_conj, c("caseF"="inv"))
table(foy_inv_conj[ ,c("inv","quifoy")],useNA="ifany")
# On ne garde donc que les caseF des "vous"
foy_inv_conj   <- foy_inv_conj[foy_inv_conj$quifoy=="vous",c("idfoy","inv")]
table(foy_inv_conj[ ,c("inv")],useNA="ifany")
invalides_conj <- invalides[invalides$quifoy=="conj",c("idfoy","noindiv")]
invalides_conj <- merge(invalides_conj, foy_inv_conj, by="idfoy", all.x=TRUE)
table(invalides_conj$inv) # TODO en 2006 On en a 316 au lieu de 328 il doit y avoir de idfoy avec caseF qui n'ont pas de vous because double déclaration'   
invalides[invalides$quifoy=="conj",c("idfoy","noindiv","inv")] <- invalides_conj
table(invalides[,c("inv","quifoy")],useNA="ifany")
rm(invalides_conj,foy_inv_conj)

# Enfants invalides et garde alternée

loadTmp("pacIndiv.Rdata")
foy_inv_pac <- invalides[!(invalides$quifoy %in% c("vous","conj")),c("inv","noindiv")] 
foy_inv_pac <- merge(foy_inv_pac, pacIndiv[,c("noindiv","typ","naia")], by="noindiv",all.x =TRUE)
names(foy_inv_pac)
table(foy_inv_pac[,c("typ","naia")],useNA="ifany")
table(foy_inv_pac[,c("typ")],useNA="ifany")
foy_inv_pac <- within(foy_inv_pac,{
  inv  <- (typ=="G") | (typ=="R") | (typ=="I") | (typ=="F" & (as.numeric(year)-naia>18))
  alt  <- (typ=="H") | (typ=="I")
  naia <- NULL
  typ  <- NULL})

table(foy_inv_pac[ ,c("inv")],useNA="ifany")
table(foy_inv_pac[ ,c("alt")],useNA="ifany")
invalides$alt <- 0
foy_inv_pac[is.na(foy_inv_pac$alt),"alt"] <- 0
invalides[!(invalides$quifoy %in% c("vous","conj")),c("noindiv","inv","alt")] <- foy_inv_pac
table(invalides$inv==1,useNA="ifany")
table(invalides$alt==1,useNA="ifany")
rm(foy_inv_pac,pacIndiv)

# Initialisation des NA sur alt et inv
invalides[is.na(invalides$inv), "inv"] <- 0
table(invalides[,c("alt","inv")],useNA="ifany")

final <- merge(final, invalides[,c("noindiv","inv","alt")], by="noindiv",all.x=TRUE)
table(final[, c("inv","alt")],useNA="ifany")

rm(invalides)
saveTmp(final, file= "final.Rdata")