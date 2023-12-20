library(reshape2)
library(vegan)
#--------------------------------------------------------------------------------------
#'
#' Cluster chemicals by their hit patterns
#'
#--------------------------------------------------------------------------------------
httrVsChemClassHitClusters.step2 <- function(dataset="MCF7 Screen",nmax=1000) {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")

  cat("read the chemclass data\n")
  file = paste0(dir,"chemclass level 2.xlsx")
  chemclass = read.xlsx(file)
  rownames(chemclass) = chemclass$dtxsid
  chems = chemclass[,c("dtxsid","name","chosen_class")]
  chems = chems[order(chems$chosen_class),]
  rownames(chems) = chems$dtxsid

  file = paste0(dir,"httr/httrVsChemClassHitClusters matrix ",dataset,".RData")
  load(file=file) # gets  hitFractionMatrix
  print(dim(hitFractionMatrix))

  exclude.list = c(
    "CALM1|DRD1|DRD2",
    "ADRA1A|ADRA1B|DRD1|DRD2|DRD3|DRD4|HTR2A|HTR2C",
    "DRD2","Prostaglandin","STAT","ATPase","E2F1"
  )
  nlist = names(hitFractionMatrix)
  nlist = nlist[!is.element(nlist,exclude.list)]
  hitFractionMatrix = hitFractionMatrix[,nlist]
  print(dim(hitFractionMatrix))
  dlist = rownames(hitFractionMatrix)
  chems = chems[dlist,]
  rs = rowSums(hitFractionMatrix)
  hitFractionMatrix = hitFractionMatrix[rs>0,]
  print(dim(hitFractionMatrix))

  #val12 = sum(hitFractionMatrix["DTXSID0020573",]*hitFractionMatrix["DTXSID0020814",])

  dmat = as.matrix(hitFractionMatrix)%*%t(as.matrix(hitFractionMatrix))
  chems = chems[rownames(dmat),]
  res = NULL
  for(i in 1:nrow(chems)) {
    d1 = chems[i,"dtxsid"]
    n1 = chems[i,"name"]
    c1 = chems[i,"chosen_class"]
    x1 = dmat[,d1]
    x1 = sort(x1,decreasing=T)
    x2 = x1[1:nmax]
    if(length(x2)>0) {
      x3 = as.data.frame(x2)
      x3$d2 = rownames(x3)
      names(x3) = c("dist","d2")
      x3$d1 = d1
      x3$n1 = n1
      x3$c1 = c1
      csub = chems[x3$d2,]
      x3$n2 = csub$name
      x3$c2 = csub$chosen_class
      nlist = c("d1","n1","c1","d2","n2","c2","dist" )
      x3 = x3[,nlist]
      res = rbind(res,x3)
    }
    if(i%%100==0) cat("finished",i,"out of",nrow(chems),"\n")
  }
  res$match = 0
  res[res$c1==res$c2,"match"] = 1
  file = paste0(dir,"httr/httrVsChemClassHitClusters similarities ",dataset,".xlsx")
  write.xlsx(res,file)
  file = paste0(dir,"httr/httrVsChemClassHitClusters similarities ",dataset,".RData")
  save(res,file=file)
}
