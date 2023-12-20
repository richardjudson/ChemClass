library(rcdk)
#'
#' Prepare the CDK descriptors for all of the SMILES
#'
cdk.prep <- function() {
  printCurrentFunction()
  dir = "data/input/SMILES/"
  file = paste0(dir,"all_smiles.xlsx")
  slist = read.xlsx(file)
  slist = slist[slist$smiles!="N/A",]
  #slist = slist[1:10000,]
  chems = slist[,c("dtxsid","name","smiles")]
  rownames(chems) = chems$dtxsid
  cat("get rid of the Markush structures\n")
  for(i in 1:nrow(slist)) slist[i,"smiles"] =  gsub('\\|.*\\|',"", slist[i,"smiles"])
  cat("create the molecule structures\n")
  molecules = parse.smiles(slist$smiles,omit.nulls = TRUE)
  slist.bad = slist[!is.element(slist$smiles,names(molecules)),]
  cat("create chems\n")
  chems = slist[is.element(slist$smiles,names(molecules)),]
  cat("create fingerprints\n")
  fps = lapply(molecules, get.fingerprint, type='extended')
  nm = length(fps)

  cat("fill in the fingerprint matrix\n")
  res = as.data.frame(matrix(nrow=nm,ncol=1024))
  res[] = 0
  if(nm!=nrow(chems)) browser()
  for(i in 1:nm) {
    smiles = names(fps[i])
    dtxsid = chems[i,"dtxsid"]
    name = chems[i,"name"]
    x = fps[[i]]
    bits = x@bits
    res[i,bits] = 1
    if(i%%1000==0) cat("finished",i,"out of",nm,"\n")
  }
  fp = cbind(chems,res)
  file = paste0(dir,"all_fingerprints.RData")
  save(fp,file=file)
 }


