#'
#' Prepare the ClassyFire results as an RData file
#'
clasyfire.prep <- function() {
  printCurrentFunction()
  dir = "data/input/classyfire/"
  file = paste0(dir,"classyfied_result_600k_part1.tsv")
  x1 = read.delim(file)
  file = paste0(dir,"results_part_2.tsv")
  x2 = read.delim(file)
  ClassyFire = rbind(x1,x2)
  nlist = c("sid","smiles","kingdom","superklass","klass","subklass","direct_parent","geometric_descriptor")
  ClassyFire = ClassyFire[,nlist]
  nlist = c("dtxsid","smiles","kingdom","superclass","class","subclass","direct_parent","geometric_descriptor")
  names(ClassyFire) = nlist
  browser()

  file = paste0(dir,"ClassyFire DSSTox.RData")
  save(ClassyFire,file=file)

}
