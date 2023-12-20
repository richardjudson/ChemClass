#'
#' Prepare the CDK descriptors for all of the SMILES
#'
nearest.neighbors.rectangle <- function(class.prefix="Pharmaceutical unknown MOA") {
  printCurrentFunction()
  dir = "data/input/"
  file = paste0(dir,"SMILES/all_fingerprints.RData")
  load(file=file)
  rownames(fp) = fp$dtxsid
  file = paste0(dir,"chemclass level 2.xlsx")
  mat = read.xlsx(file)
  mat = mat[!is.na(mat$chosen_class),]
  rownames(mat) = mat$dtxsid
  fp = fp[is.element(fp$dtxsid,mat$dtxsid),]
  x = mat[grepl(class.prefix,mat$chosen_class),]
  dlist = x$dtxsid
  dlist = dlist[is.element(dlist,fp$dtxsid)]
  fp0 = fp[,5:ncol(fp)]

  nlist = c("d1","n1","c1","d2","n2","c2","match","dist")
  res = as.data.frame(matrix(nrow=length(dlist),ncol=length(nlist)))
  names(res) = nlist
  for(i in 1:length(dlist)) {
    d1 = dlist[i]
    fp1 = fp0[d1,]

    vals = apply(X=fp0,MARGIN=1,FUN=dist0,fp1)
    y = sort(vals)
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
  file = paste0(dir,"NN ",class.prefix,".xlsx")
  write.xlsx(res,file)
}
dist0 <- function(x,y) {
  x = as.numeric(x)
  y = as.numeric(y)
  top = x*y
  bot = x+y
  bot[bot>1] = 1
  val = 1-sum(top)/sum(bot)
  return(val)
}
