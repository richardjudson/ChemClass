library(httr)
#'
#' grab the new classyfire classifictions
#'
classyfire.new <- function() {
  printCurrentFunction()
  dir = "data/input/"

  file = "data/input/chemclass universe.xlsx"
  print(file)
  tbd = read.xlsx(file)
  tbd = tbd[,c("dtxsid","name")]
  tbd$success = 0
  tbd$kingdom	= NA
  tbd$superclass = NA
  tbd$class	= NA
  tbd$subclass = NA
  for(i in 1:nrow(tbd)) {
    dtxsid = tbd[i,"dtxsid"]
    query = paste0("https://hazard-dev.sciencedataexperts.com/api/resolver/classyfire?query=",dtxsid)
    resp = GET(query, mode="excel")
    if(http_status(resp)$category=="Success") {
      x = content(resp)$content
      n = length(x)
      if(length(x)>0) {
        tbd[i,"kingdom"] = x[[1]]$kingdom
        tbd[i,"superclass"] = x[[1]]$superklass
        tbd[i,"class"] = x[[1]]$klass
        tbd[i,"subclass"] = x[[1]]$subklass
        tbd[i,"success"] = 1
        #cat(tbd[i,"name"],"\n")
      }
    }
    if(i%%1000==0) cat("finished",i,"out of",nrow(tbd),"\n")
  }
  cat(nrow(tbd),":",sum(tbd$success),"\n")
  browser()
  file = "data/input/chemclass classyfire.xlsx"
  write.xlsx(tbd,file)
}



