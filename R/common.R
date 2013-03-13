# Usefuls function

library(Hmisc)
library(car)


LoadIn <- function(file,vars=NULL){
  tmp <- load(file);
  if (is.null(vars)){
    out <- get(tmp);}
  else{ 
    out <- get(tmp)[vars];
  }
}


# Some useful functions
saveTmp <- function(data, file = stop("'file' must be specified")){
  name <- deparse(substitute(data))
  path <- paste(tmpDir, file, sep = "")
  save(list = name, file = path)
}

loadTmp <- function(file){
  path <- paste(tmpDir, file, sep = "")
  load(path, globalenv())
}

delTmp <- function(){
  # deletes all files in tmpDir
  files <- dir(tmpDir)
  for (file in files){
    path <- paste(tmpDir, file, sep = "")
    file.remove(path)    
  }
}

print_id <- function(dts){
  print("Individus")
  print(length(table(dts$noindiv)))
  
  # Ici, il doit y avoir autant de vous que d'idfoy
  print("Foyers")
  print(length(table(dts$idfoy)))
  print(table(dts$quifoy,useNA="ifany"))
  
  # Ici, il doit y avoir autant de quimen = 0 que d'idmen
  print("Ménages")
  print(length(table(dts$idmen)))
  print(table(dts$quimen,useNA="ifany"))
  
  # Ici, il doit y avoir autant de quifam = 0 que d'idfam
  print("Familles")
  print(length(table(dts$idfam)))
  print(table(dts$quifam,useNA="ifany"))
}
