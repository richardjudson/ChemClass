#'
#' Prepare the CDK descriptors for all of the SMILES
#'
nearest.neighbors <- function(class.prefix="Antibiotic") {
  printCurrentFunction()
  dir = "data/input/"
  file = paste0(dir,"SMILES/all_fingerprints.RData")
  load(file=file)
  rownames(fp) = fp$dtxsid
  file = paste0(dir,"chemclass level 2.xlsx")
  mat = read.xlsx(file)
  rownames(mat) = mat$dtxsid
  x = mat[grepl(class.prefix,mat$chosen_class),]
  dlist = x$dtxsid
  subset = T
  if(is.element(class.prefix,c("Unclassed","Pharmaceutical unknown MOA"))) subset = F

  if(subset) fp = fp[is.element(fp$dtxsid,x$dtxsid),]
  fp0 = fp[,5:ncol(fp)]
  dlist = rownames(fp)
  dmat = dist(fp0,method="binary")
  dmat = as.matrix(dmat)

  nlist = c("d1","n1","c1","d2","n2","c2","match","dist")
  res = as.data.frame(matrix(nrow=length(dlist),ncol=length(nlist)))
  names(res) = nlist
  for(i in 1:length(dlist)) {
    d1 = dlist[i]
    #d1 = rownames(dmat)[i]
    y = sort(dmat[,d1])
    d2 = names(y)[2]
    val = y[2]
    res[i,"d1"] = d1
    res[i,"d2"] = d2
    res[i,"dist"] = val
    res[i,"n1"] = fp[d1,"name"]
    res[i,"n2"] = fp[d2,"name"]
    res[i,"c1"] = mat[d1,"chosen_class"]
    res[i,"c2"] = mat[d2,"chosen_class"]
    res[i,"match"] = 0
    if(res[i,"c1"]==res[i,"c2"]) res[i,"match"] = 1
  }

  browser()
  file = paste0(dir,"NN ",class.prefix,".xlsx")
  write.xlsx(res,file)
}
