#'
#' Get the chemical classes for OASIS
#'
oasis.classes <- function() {
  printCurrentFunction()
  dir = "data/input/"
  file = paste0(dir,"chemclass level 2.xlsx")
  chemclass = read.xlsx(file)
  file = paste0(dir,"ToxRefdb_rat_subchronic_micro_liver_chem_ids.xlsx")
  oasis = read.xlsx(file)
  print(nrow(oasis))
  oasis = unique(oasis)
  oasis = oasis[,c("dtxsid","name")]
  print(nrow(oasis))
  oasis$class_type = NA
  oasis$chosen_class = NA
  rownames(oasis) = oasis$dtxsid
  rownames(chemclass) = chemclass$dtxsid

  for(i in 1:nrow(oasis)) {
    dtxsid = oasis[i,"dtxsid"]
    if(is.element(dtxsid,chemclass$dtxsid)) {
      oasis[i,"class_type"] = chemclass[dtxsid,"class_type"]
      oasis[i,"chosen_class"] = chemclass[dtxsid,"chosen_class"]
    }
  }
  #browser()
  file = paste0(dir,"OASIS chemical classes 2023-12-20.xlsx")
  write.xlsx(oasis,file)
}
