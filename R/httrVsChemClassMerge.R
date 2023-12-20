library(ggplot2)
library(gplots)
library(RColorBrewer)
#--------------------------------------------------------------------------------------
#'
#' Merge HTTr hit rates vs the chemical categories
#'
#--------------------------------------------------------------------------------------
httrVsChemClassMerge <- function() {
  printCurrentFunction()
  dir = "data/input/httr/"
  par(mfrow=c(1,1),mar=c(4,4,2,2))
  dataset = "MCF7 Screen"
  file = paste0(dir,"HTTrChemclassStats ",dataset,".xlsx")
  mat1 = read.xlsx(file)
  dataset = "U2OS Screen"
  file = paste0(dir,"HTTrChemclassStats ",dataset,".xlsx")
  mat2 = read.xlsx(file)
  dataset = "HepaRG Screen"
  file = paste0(dir,"HTTrChemclassStats ",dataset,".xlsx")
  mat3 = read.xlsx(file)

  clist = sort(unique(c(mat1$chosen_class,mat2$chosen_class,mat3$chosen_class)))
  dlist = sort(unique(c(mat1$dtxsid,mat2$dtxsid,mat3$dtxsid)))


  nlist = c("dtxsid","name","chosen_class","MCF7","U2OS","HepaRG")
  res = as.data.frame(matrix(nrow=length(dlist),ncol=length(nlist)))
  names(res) = nlist
  for(i in 1:length(dlist)) {
    dtxsid = dlist[i]
    res[i,"dtxsid"] = dtxsid
    x = mat1[is.element(mat1$dtxsid,dtxsid),]
    if(nrow(x)==1) {
      res[i,"name"] = x[1,"name"]
      res[i,"chosen_class"] = x[1,"chosen_class"]
      res[i,"MCF7"] = x[1,"nhit"]
    }
    x = mat2[is.element(mat2$dtxsid,dtxsid),]
    if(nrow(x)==1) {
      res[i,"name"] = x[1,"name"]
      res[i,"chosen_class"] = x[1,"chosen_class"]
      res[i,"U2OS"] = x[1,"nhit"]
    }
    x = mat3[is.element(mat3$dtxsid,dtxsid),]
    if(nrow(x)==1) {
      res[i,"name"] = x[1,"name"]
      res[i,"chosen_class"] = x[1,"chosen_class"]
      res[i,"HepaRG"] = x[1,"nhit"]
    }

  }
  file = paste0(dir,"HTTrChemclassStats merged.xlsx")
  write.xlsx(res,file)
}

