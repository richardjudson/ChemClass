library(reshape2)
#--------------------------------------------------------------------------------------
#'
#' Analyze the HTTr data vs the chemical categories
#' Run httrVsChemClassHitClusters to generate the input data file
#'
#--------------------------------------------------------------------------------------
httrVsChemClassHitPatterns <- function(to.file=F,dataset="MCF7 Screen",classlist=c("Estrogen","Bisphenol")) {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")

  label = ""
  for(i in 1:length(classlist)) label = paste(label,substr(classlist[i],1,10))
  #label=paste(classlist,collapse=" ")
  if(to.file) {
    fname <- paste0(dir,"httr/httrVsChemClassHitPatterns ",label,".pdf")
    pdf(file=fname,width=8,height=8,pointsize=12,bg="white",paper="letter",pagecentre=T)
  }


  file = paste0(dir,"httr/httrVsChemClassHitClusters matrix ",dataset,".RData")
  load(file=file) # gets  hitFractionMatrix
  exclude.list = c(
    "CALM1|DRD1|DRD2",
    "ADRA1A|ADRA1B|DRD1|DRD2|DRD3|DRD4|HTR2A|HTR2C",
    "DRD2","Prostaglandin","STAT","ATPase","E2F1"
  )
  nlist = names(hitFractionMatrix)
  nlist = nlist[!is.element(nlist,exclude.list)]
  hitFractionMatrix = hitFractionMatrix[,nlist]

  dlist = rownames(hitFractionMatrix)
  file = paste0(dir,"chemclass level 2.xlsx")
  chemclass = read.xlsx(file)
  rownames(chemclass) = chemclass$dtxsid
  chemclass = chemclass[is.element(chemclass$dtxsid,dlist),]
  chems = chemclass[is.element(chemclass$chosen_class,classlist),c("dtxsid","name","chosen_class")]
  chems = chems[order(chems$chosen_class),]
  rownames(chems) = chems$dtxsid

  t2 = hitFractionMatrix[chems$dtxsid,]
  cs = colSums(t2)
  t2 = t2[,cs>0]
  t3 = as.data.frame(t(t2))
  names(t3) = chems[names(t3),"name"]
  t3=t3[,chems$name]
  # browser()

  rowmax = apply(t3, 1, max, na.rm=TRUE)
  t3 = t3[rowmax>0.2,]
  rowmin = apply(t3, 1, min, na.rm=TRUE)
  cols = names(t3)
  clist = c("red","orange","yellow","green","blue","violet","black","gray","khaki")


  cols[]="white"
  for(i in 1:length(classlist)) {
    classi = classlist[i]
    chemsi = chems[chems$chosen_class==classi,"name"]
    cols[is.element(names(t3),chemsi)] = clist[i]
  }
  t3[t3>1] = 1
  heatmap.2(as.matrix(t3),Colv=F,Rowv=T,trace="none",margins=c(15,15),cexRow=0.3,cexCol=0.3,dendrogram="row",
            col=brewer.pal(n = 9, "Reds"),ColSideColors=cols,main=label)

  if(!to.file) browser()
  else dev.off()

}
