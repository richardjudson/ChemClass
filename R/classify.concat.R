library(classyfireR)
library(R.utils)
#-------------------------------------------------------------------------------
#' Concatenates all of the classyfire output files
#'
#' @param batchsset The batch version for their directory
#-------------------------------------------------------------------------------
classify.concat <- function(batchset=2) {
  printCurrentFunction()
  dir=paste0("data/input/classyfire/batches set ",batchset,"/")
  flist = list.files(dir)

  res = NULL
  for(fname in flist) {
    file = paste0(dir,fname)
    temp = read.xlsx(file)
    temp = temp[!is.na(temp$dtxsid),]
    cat(fname,nrow(temp),"\n")
    res = rbind(res,temp)
  }

  file = paste0("data/input/classyfire/ClassyFire all classifications v1.xlsx")
  v1 = read.xlsx(file)
  v1 = v1[]
  res = rbind(res,v1)
  file = paste0("data/input/classyfire/classification set ",batchset,".xlsx")
  write.xlsx(res,file)

}
