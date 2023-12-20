#'
#' map the putative classes for the new drugs and pesticides
#'
find.chems.by.class.mapper <- function() {
  printCurrentFunction()

  dir = "data/input/"
  file = paste0(dir,"candidate drugs and pesticides.xlsx")
  cand = read.xlsx(file)

  file = paste0(dir,"drug classes v2.xlsx")
  print(file)
  drug = read.xlsx(file)
  file = "data/input/pesticide classes v2.xlsx"
  print(file)
  pest = read.xlsx(file)

  d2 = cand[cand$class=="Drug",]
  d2 = unique(d2[,c("dtxsid","name")])
  d2$drug_class = NA
  d2$source_chem = NA
  for(i in 1:nrow(d2)) {
    dtxsid = d2[i,"dtxsid"]
    dlist = cand[cand$dtxsid==dtxsid,"d1"]
    temp = drug[is.element(drug$dtxsid,dlist),]
    d2[i,"drug_class"] = paste(sort(unique(temp$drug_class)),collapse="|")
    d2[i,"source_chem"] = paste(sort(unique(temp$name)),collapse="|")
  }
  file = paste0(dir,"drug candidates.xlsx")
  write.xlsx(d2,file)

  p2 = cand[cand$class=="Pesticide",]
  p2 = unique(p2[,c("dtxsid","name")])
  p2$structure_class = NA
  p2$pesticide_class = NA
  for(i in 1:nrow(p2)) {
    dtxsid = p2[i,"dtxsid"]
    dlist = cand[cand$dtxsid==dtxsid,"d1"]
    temp = pest[is.element(pest$dtxsid,dlist),]
    p2[i,"structure_class"] = paste(sort(unique(temp$structure_class)),collapse="|")
    p2[i,"pesticide_class"] = paste(sort(unique(temp$pesticide_class)),collapse="|")
    p2[i,"source_chem"] = paste(sort(unique(temp$name)),collapse="|")
  }
  file = paste0(dir,"pesticide candidates.xlsx")
  write.xlsx(p2,file)


}
