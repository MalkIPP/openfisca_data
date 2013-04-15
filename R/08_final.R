##***********************************************************************/
message('08_final: derniers réglages')
##***********************************************************************/


loadTmp("final.Rdata")
# On définit comme célibataires les individus dont on n'a pas retrouvé la déclaration
final$statmarit[is.na(final$statmarit)] <- 2
table(final$statmarit, useNA='ifany')

# recode quifoy # TODO: should have been done directly above
table(final$quifoy, useNA="ifany")
levels(final$quifoy) <- list("0"='vous', 
                             "1"='conj',
                             "2"='pac1',
                             "3"='pac2',
                             "4"='pac3',
                             "5"='pac4',
                             "6"='pac5',
                             "7"='pac6',
                             "8"='pac7',
                             "9"='pac8')

final$quifoy <- as.numeric(levels(final$quifoy)[final$quifoy])

# recode quimen # TODO: should have been done directly above
table(final$quimen, useNA="ifany")
levels(final$quimen) <- list("0"="0", 
                             "1"="1",
                             "2"='enf1',
                             "3"='enf2',
                             "4"='enf3',
                             "5"='enf4',
                             "6"='enf5',
                             "7"='enf6',
                             "8"='enf7',
                             "9"='enf8',
                             "10"='enf9')
str(final$quimen)
final$quimen <- as.numeric(levels(final$quimen)[final$quimen])


# activite des fip
table(final[final$quelfic=="FIP","activite"],useNA="ifany")
summary(final[final$quelfic=="FIP",c("activite","choi","sali","alr","rsti","age")] )
# activite      # actif occup? 0, ch?meur 1, ?tudiant/?l?ve 2, retrait? 3, autre inactif 4

final_fip <- final[final$quelfic=="FIP",]
final_fip <- within(final_fip,{
  choi <- ifelse(is.na(choi),0,choi)
  sali <- ifelse(is.na(sali),0,sali)
  alr <- ifelse(is.na(alr),0,alr)
  rsti <- ifelse(is.na(rsti),0,rsti)
  activite <- 2 # TODO comment choisr la valeur par d?faut ?
  activite <- ifelse(choi > 0,1,activite)
  activite <- ifelse(sali > 0,0,activite)
  activite <- ifelse(age  >= 21, 2,activite) # ne peuvent être rattach?s que les ?tudiants  
})
final[final$quelfic=="FIP",]<- final_fip
table(final_fip[,c("age","activite")])
rm(final_fip)

print_id(final)
saveTmp(final, file= "final.Rdata")

loadTmp("final.Rdata")
load(menm)
menagem <- rename(menagem, c("ident"="idmen","loym"="loyer"))
menagem$cstotpragr <- floor(menagem$cstotpr/10)



# 2008 tau99 removed TODO: check ! and check incidence
if (year == "2008") {
 vars <- c("loyer", "tu99", "pol99", "reg","idmen", "so", "wprm", "typmen15", "nbinde","ddipl","cstotpragr","champm","zthabm")
} else {
  vars <- c("loyer", "tu99", "pol99", "tau99", "reg","idmen", "so", "wprm", "typmen15", "nbinde","ddipl","cstotpragr","champm","zthabm")
}

famille_vars <- c("m_afeamam", "m_agedm","m_clcam", "m_colcam", 'm_mgamm', 'm_mgdomm')
if ("naf16pr" %in% names(menagem)) {
  naf16pr <- factor(menagem$naf16pr)
  levels(naf16pr) <-  0:16
  menagem$naf16pr <- as.character(naf16pr)
  menagem[is.na(menagem$naf16pr), "naf16pr" ] <- "-1"  # Sans objet 
  vars <- c(vars,"naf16pr")
} else if ("nafg17npr" %in% names(menagem)) {
  # TODO: pb in 2008 with xx
  if (year == "2008"){
    menagem[ menagem$nafg17npr == "xx" & !is.na(menagem$nafg17npr), "nafg17npr"] <- "00"
  }
  nafg17npr <- factor(menagem$nafg17npr)  
  levels(nafg17npr) <-  0:17
  menagem$nafg17npr <- as.character(nafg17npr)
  menagem[is.na(menagem$nafg17npr), "nafg17npr" ] <- "-1"  # Sans objet
}

# TODO: 2008tau99 is not present should be provided by 02_loy.... is it really needed
all_vars <- union(vars,famille_vars)
available_vars <- all_vars[union(vars,famille_vars) %in% names(menagem)]
loyersMenages <- menagem[,available_vars]


# Recodage de typmen15: modalités de 1:15
table(loyersMenages$typmen15, useNA="ifany")
loyersMenages <- within(loyersMenages, {
  typmen15[typmen15==10 ] <- 1
  typmen15[typmen15==11 ] <- 2
  typmen15[typmen15==21 ] <- 3
  typmen15[typmen15==22 ] <- 4
  typmen15[typmen15==23 ] <- 5
  typmen15[typmen15==31 ] <- 6
  typmen15[typmen15==32 ] <- 7
  typmen15[typmen15==33 ] <- 8
  typmen15[typmen15==41 ] <- 9
  typmen15[typmen15==42 ] <- 10
  typmen15[typmen15==43 ] <- 11
  typmen15[typmen15==44 ] <- 12
  typmen15[typmen15==51 ] <- 13
  typmen15[typmen15==52 ] <- 14
  typmen15[typmen15==53 ] <- 15
})



# Pb avec ddipl, pas de modalités 2: on décale les chaps >=3
# Cependant on fait cela après avoir fait les traitement suivants
table(loyersMenages$ddipl, useNA="ifany")
# On convertit les ddipl en numeric
loyersMenages$ddipl <- as.numeric(loyersMenages$ddipl)
table(loyersMenages$ddipl, useNA="ifany")
#   On met les non renseignés ie, NA et "" à sans diplome (modalité 7)
loyersMenages[is.na(loyersMenages$ddipl), "ddipl"] <- 7

loyersMenages[loyersMenages$ddipl>1, "ddipl"] <- loyersMenages$ddipl[loyersMenages$ddipl>1]-1


table(final$actrec,useNA="ifany")
final$act5 <- NA    
final <- within(final, {
  act5[which(actrec==1) ] <- 2 # ind?pendants
  act5[which(actrec==2) ] <- 1 # salari?s
  act5[which(actrec==3) ] <- 1 # salari?s
  act5[which(actrec==4) ] <- 3 # ch?meur
  act5[which(actrec==7) ] <- 4 # retrait?
  act5[which(actrec==8) ] <- 5 # autres inactifs
})
table(final$act5,useNA="ifany")

final$wprm <- NULL # with the intention to extract wprm from menage to deal with FIPs
final$tax_hab <- final$zthabm # rename zthabm to tax_hab
final$zthabm <- NULL

final2 <- merge(final, loyersMenages, by="idmen", all.x=TRUE)

# TODO: merging with patrimoine
rm(menagem,final)

# table(final2$activite,useNA="ifany")
# table(final2$alt,useNA="ifany")

saveTmp(final2, file= "final2.Rdata")

loadTmp("final2.Rdata")
names(final2)
print_id(final2)


# set zone_apl using zone_apl_imputation_data
apl_imp <- read.csv("./zone_apl/zone_apl_imputation_data.csv")

if (year == "2008") {
  zone_apl <- final2[, c("tu99", "pol99", "reg")]
} else {
  zone_apl <- final2[, c("tu99", "pol99", "tau99", "reg")]
}

for (i in 1:length(apl_imp[,"TU99"])) {
  tu <- apl_imp[i,"TU99"]
  pol <- apl_imp[i,"POL99"]
  tau <- apl_imp[i,"TAU99"]
  reg <- apl_imp[i,"REG"]
  #  print(c(tu,pol,tau,reg))
  
  if (year == "2008") {
    indices <- (final2["tu99"] == tu & final2["pol99"] == pol  & final2["reg"] == reg)
    selection <-  (apl_imp["TU99"] == tu & apl_imp["POL99"] == pol & apl_imp["REG"] == reg)
  } else {
    indices <- (final2["tu99"] == tu & final2["pol99"] == pol & final2["tau99"] == tau & final2["reg"] == reg)
    selection <-  (apl_imp["TU99"] == tu & apl_imp["POL99"] == pol & apl_imp["TAU99"] == tau & apl_imp["REG"] == reg) 
  }
  z <- runif(sum(indices))
  probs <- apl_imp[selection , c("proba_zone1", "proba_zone2")]
  #  print(probs)
  final2[indices,"zone_apl"] <- 1 + (z>probs[,'proba_zone1']) + (z>(probs[,'proba_zone1']+probs[,'proba_zone2']))
  rm(indices, probs)
}


# var <- names(foyer)
#a1 <- c('f7rb', 'f7ra', 'f7gx', 'f2aa', 'f7gt', 'f2an', 'f2am', 'f7gw', 'f7gs', 'f8td', 'f7nz', 'f1br', 'f7jy', 'f7cu', 'f7xi', 'f7xo', 'f7xn', 'f7xw', 'f7xy', 'f6hj', 'f7qt', 'f7ql', 'f7qm', 'f7qd', 'f7qb', 'f7qc', 'f1ar', 'f7my', 'f3vv', 'f3vu', 'f3vt', 'f7gu', 'f3vd', 'f2al', 'f2bh', 'f7fm', 'f8uy', 'f7td', 'f7gv', 'f7is', 'f7iy', 'f7il', 'f7im', 'f7ij', 'f7ik', 'f1er', 'f7wl', 'f7wk', 'f7we', 'f6eh', 'f7la', 'f7uh', 'f7ly', 'f8wy', 'f8wx', 'f8wv', 'f7sb', 'f7sc', 'f7sd', 'f7se', 'f7sf', 'f7sh', 'f7si',  'f1dr', 'f7hs', 'f7hr', 'f7hy', 'f7hk', 'f7hj', 'f7hm', 'f7hl', 'f7ho', 'f7hn', 'f4gc', 'f4gb', 'f4ga', 'f4gg', 'f4gf', 'f4ge', 'f7vz', 'f7vy', 'f7vx', 'f7vw', 'f7xe', 'f6aa', 'f1cr', 'f7ka', 'f7ky', 'f7db', 'f7dq', 'f2da')
#a2 <- setdiff(a1,names(foyer))
#b1 <- c('pondfin', 'alt', 'hsup', 'ass_mat', 'zone_apl', 'inactif', 'ass', 'aer', 'code_postal', 'activite', 'type_sal', 'jour_xyz', 'boursier', 'etr', 'partiel1', 'partiel2', 'empl_dir', 'gar_dom', 'categ_inv', 'opt_colca', 'csg_taux_plein','coloc') 
# hsup feuille d'impot
# boursier pas dispo
# inactif etc : extraire cela des donn?es clca etc

# tester activit? car 0 vaut actif
table(is.na(final2$activite),useNA="ifany")

saveTmp(final2, file= "final2.Rdata")