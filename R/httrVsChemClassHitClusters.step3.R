library(reshape2)
library(vegan)
#--------------------------------------------------------------------------------------
#'
#' Cluster chemicals by their hit patterns
#'
#--------------------------------------------------------------------------------------
httrVsChemClassHitClusters.step3 <- function(dataset="MCF7 Screen",nmax = 1000) {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")

  file = paste0(dir,"chemclass level 2.xlsx")
  print(file)
  chemclass = read.xlsx(file)
  rownames(chemclass) = chemclass$dtxsid
  chems = chemclass[,c("dtxsid","name","chosen_class")]
  chems = chems[order(chems$chosen_class),]
  rownames(chems) = chems$dtxsid

  file = paste0(dir,"httr/httrVsChemClassHitClusters similarities ",dataset,".RData")
  print(file)
  #res = read.xlsx(file)
  load(file=file)

  chems = chems[is.element(chems$dtxsid,res$d1),]
  r2 = chems
  r2$minmatch = NA
  cat("start loop\n")
  for(i in 1:nrow(chems)) {
    dtxsid = chems[i,"dtxsid"]
    cclass = chems[i,"chosen_class"]
    t2 = res[res$d1==dtxsid,]
    t2 = t2[!is.element(t2$d2,dtxsid),]
    t3 = t2[order(t2$dist,decreasing=T),]
    t3$order = seq(from=1,to=nrow(t3))
    t4 = t3[t3$c2==cclass,]
    if(nrow(t4)==0) r2[i,"minmatch"] = nmax
    else r2[i,"minmatch"] = t4[1,"order"]
    if(i%%100==0) cat("finished",i,"out of",nrow(chems),"\n")
  }
  file = paste0(dir,"httr/httrVsChemClassHitClusters minmatch ",dataset,".xlsx")
  write.xlsx(r2,file)
}
