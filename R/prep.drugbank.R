library(stringr)
library(stringi)
#--------------------------------------------------------------------------------------
#
# load the DrugBank data
#
#--------------------------------------------------------------------------------------
prep.drugbank <- function(do.read=T) {
  printCurrentFunction()
  dir = "data/input/drugs/"
  if(do.read) {
    file = paste0(dir,"drugbank_index_with_mode_2022-04-19.xlsx")
    DBMAT <<- read.xlsx(file)
  }
  db.list = sort(unique(DBMAT[,"drugcard_id"]))
  ndb = length(db.list)
  nlist = c("generic_name","brand_name","inchikey","drug_category")
  row = as.data.frame(matrix(nrow=1,ncol=length(nlist)))
  names(row) = nlist
  res = NULL
  for(i in 1:ndb) {
    dbid = db.list[i]
    temp = DBMAT[is.element(DBMAT[,"drugcard_id"],dbid),]
    temp = unique(temp)
    casrn = fix.casrn(temp[is.element(temp[,"name"],"casrn"),"value"])
    inchikey = temp[is.element(temp[,"name"],"inchi_key"),"value"]
    generic_name = NA
    brand_name = NA

    if(is.element("generic_name",temp[,"name"])) generic_name = temp[is.element(temp[,"name"],"generic_name"),"value"]
    if(is.element("brand_name",temp[,"name"])) brand_name = temp[is.element(temp[,"name"],"brand_name"),"value"]
    generic_name = str_replace_all(generic_name,"'","''")
    brand_name = str_replace_all(brand_name,"'","''")
    temp = temp[!is.element(temp[,"value"],"Not Available"),]
    if(is.element("drug_category",temp[,"name"])) {
      dcat = temp[is.element(temp[,"name"],"drug_category"),"value"]
      dcat = str_replace_all(dcat,"'","")
      dcat = unique(dcat)
      dcat = dcat[1]
      row[1,"drug_category"] = dcat
      row[1,"brand_name"] = brand_name
      row[1,"generic_name"] = generic_name
      row[1,"casrn"] = casrn
      row[1,"inchikey"] = inchikey
      res = rbind(res,row)
    }
  }
  file = paste0(dir,"drugbank category data.xlsx")
  write.xlsx(res,file)
}
