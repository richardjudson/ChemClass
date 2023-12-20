library(openxlsx)
#'
#' Get the Drugbank data from RefChemDB
#'
get.drugs.from.refchemdb <- function() {
  printCurrentFunction()
  res = runQuery("select a.dtxsid,b.name from chemical_target a, chemical b where a.source='DrugBank' and b.dtxsid=a.dtxsid","dev_refchemdb")
  file = "data/input/drugs/drugbank chemicals.xlsx"
  write.xlsx(res,file)
}
