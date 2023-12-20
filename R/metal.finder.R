library(stringr)
#'
#' Find the metal containing compounds
#'
metal.finder <- function() {
  printCurrentFunction()
  dir = "data/input/"
  prefix = "SMILES/CCD-Batch-Search_2023-08-31_02_00_29 batch "
  mat = NULL
  for(i in 1:8) {
    file = paste0(dir,prefix,i,".csv")
    print(file)
    t1 = read.csv(file)
    t2 = t1[,c("DTXSID","PREFERRED_NAME","SMILES","MOLECULAR_FORMULA")]
    names(t2) = c("dtxsid","name","smiles","formula")
    mat = rbind(mat,t2)
  }
  file = paste0(dir,"SMILES/all_smiles.xlsx")
  write.xlsx(mat,file)
  file = paste0(dir,"Metal_List.xlsx")
  metals = read.xlsx(file)
  mlist = metals[,2]
  mat$metal = NA
  for(element in mlist) {
    cat(element,"\n")
    x = str_detect(mat$formula,element)
    mat[x,"metal"] = element
  }
  mat = mat[!is.na(mat$metal),]
  file = paste0(dir,"metals.xlsx")
  write.xlsx(mat,file)
  browser()
}
