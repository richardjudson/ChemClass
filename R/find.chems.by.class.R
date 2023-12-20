library(stringdist)
#'
#' find chemicals that look like pesticides or drugs
#'
find.chems.by.class <- function(cutoff=0.6) {
  printCurrentFunction()

  dir = "data/input/"
  file = paste0(dir,"chemclass level 2.xlsx")
  input = read.xlsx(file)
  input0 = input
  pest = input[input$class_type=="Pesticide",]
  drug = input[input$class_type=="Drug",]
  dlist = pest$dtxsid
  input = input[!is.element(input$dtxsid,dlist),]
  dlist = drug$dtxsid
  input = input[!is.element(input$dtxsid,dlist),]
  input = input[is.element(input$class_type,c("ClassyFire","Unclassed")),]

  file = paste0(dir,"exclude list.csv")
  exclude.list = read.csv(file)[,1]


  nlist = input$name
  nlist = str_replace_all(nlist,"-"," ")
  for(exclude in exclude.list) {
    print(exclude)
    nlist = str_replace_all(nlist,exclude,"")
  }
  input$name = nlist

  #input = input[1:100,]
  res = NULL
  for(i in 1:nrow(pest)) {
    d1 = pest[i,"dtxsid"]
    n1 = pest[i,"name"]
    cat(n1,"\n")
    n2 = input$name
    x = stringsim(n1,n2)
    if(max(x)>=cutoff) {
      scores  = x[x>=cutoff]
      temp = input[x>=cutoff,c("dtxsid","name")]
      temp$d1 = d1
      temp$n1 = n1
      temp$class = "Pesticide"
      temp$sim = scores
      if(!is.null(res)) if(ncol(temp)!=ncol(res)) browser()
      res = rbind(res,temp)
    }
  }
  for(i in 1:nrow(drug)) {
    d1 = drug[i,"dtxsid"]
    n1 = drug[i,"name"]
    cat(n1,"\n")
    n2 = input$name
    x = stringsim(n1,n2)
    if(max(x)>=cutoff) {
      scores  = x[x>=cutoff]
      temp = input[x>=cutoff,c("dtxsid","name")]
      temp$d1 = d1
      temp$n1 = n1
      temp$class = "Drug"
      temp$sim = scores
      if(ncol(temp)!=ncol(res)) browser()
      res = rbind(res,temp)
    }
  }
  rownames(input0) = input0$dtxsid
  res[,"name"] = input0[res$dtxsid,"name"]
  res[,"n1"] = input0[res$d1,"name"]

  file = paste0(dir,"candidate drugs and pesticides.xlsx")
  write.xlsx(res,file)
}
