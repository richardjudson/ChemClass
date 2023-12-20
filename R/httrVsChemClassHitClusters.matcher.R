library(reshape2)
library(vegan)
#--------------------------------------------------------------------------------------
#'
#' Find chemicals that are close to chemicals in a class
#'
#' process to get here
#' httrVsChemClassHitClusters.step3.boxplot
#' httrVsChemClassHitClusters.step3
#' httrVsChemClassHitClusters.step2
#' httrVsChemClassHitClusters
#'
#--------------------------------------------------------------------------------------
httrVsChemClassHitClusters.matcher <- function(dataset="MCF7 Screen",class="Bisphenol",cutoff=20) {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")
  file = paste0(dir,"httr/httrVsChemClassHitClusters matrix ",dataset,".RData")
  load(file=file) # gets  hitFractionMatrix

  file = paste0(dir,"chemclass level 2.xlsx")
  print(file)
  chemclass = read.xlsx(file)
  rownames(chemclass) = chemclass$dtxsid
  chems = chemclass[,c("dtxsid","name","chosen_class")]
  rownames(chems) = chems$dtxsid
  chems = chems[rownames(hitFractionMatrix),]

  chems.in = chems[chems$chosen_class==class,]

  hitFractionMatrix.in = hitFractionMatrix[chems.in$dtxsid,]
  cs = colSums(hitFractionMatrix.in)
  cs = sort(cs,decreasing=T)
  useme = names(cs)[1:cutoff]
  reject = names(cs)[(cutoff+1):length(cs)]
  reference = hitFractionMatrix.in
  reference[,reject] = 0
  rvec = colSums(reference)/nrow(chems.in)
  nvec = rvec
  nvec[] = 0
  nvec[reject] = 1
  yvec = nvec
  yvec[] = 1
  yvec[reject] = 0
  svec = as.data.frame(t(yvec%*%t(as.matrix(hitFractionMatrix))))
  dvec = as.data.frame(t(nvec%*%t(as.matrix(hitFractionMatrix))))
  chems = cbind(chems,svec)
  chems = cbind(chems,dvec)
  names(chems)[4] = "in.sim"
  names(chems)[5] = "out.sim"
  chems$delta = chems$in.sim - chems$out.sim
  chems = chems[order(chems$out.sim,decreasing=T),]
  file = paste0(dir,"httr/class similarity ",class," ",dataset,".xlsx")
  write.xlsx(chems,file)
}
