#'
#' Get the counts by class
#'
use.class.counts <- function() {
  printCurrentFunction()
  dir = "data/input/"
  file = "data/input/chemclass level 2.xlsx"
  mat = read.xlsx(file)
  clist = unique(mat$chosen_class)
  res= as.data.frame(matrix(nrow=length(clist),ncol=2))
  names(res) = c("class","chemicals")
  for(i in 1:length(clist)) {
    class = clist[i]
    temp = mat[is.element(mat$chosen_class,class),]
    res[i,"class"] = class
    res[i,"chemicals"] = nrow(temp)
  }

  file = "data/input/class counts.xlsx"
  write.xlsx(res,file)
}
