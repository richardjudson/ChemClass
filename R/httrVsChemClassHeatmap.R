library(ggplot2)
library(gplots)
library(RColorBrewer)
#--------------------------------------------------------------------------------------
#'
#' Heatmap of the HTTr hit rates vs the chemical categories
#'
#--------------------------------------------------------------------------------------
httrVsChemClassHeatmap <- function(to.file=F,nmin=3,color.cut=400) {
  printCurrentFunction()
  dir = "data/input/httr/"
  if(to.file) {
    fname <- paste0(dir,"httrVsChemClassHeatmap.pdf")
    pdf(file=fname,width=8,height=8,pointsize=12,bg="white",paper="letter",pagecentre=T)
  }
  par(mfrow=c(1,1),mar=c(4,4,2,2))
  dataset = "MCF7 Screen"
  file = paste0(dir,"HTTrChemclassStats ",dataset,".xlsx")
  mat1 = read.xlsx(file)
  dataset = "U2OS Screen"
  file = paste0(dir,"HTTrChemclassStats ",dataset,".xlsx")
  mat2 = read.xlsx(file)
  dataset = "HepaRG Screen"
  file = paste0(dir,"HTTrChemclassStats ",dataset,".xlsx")
  mat3 = read.xlsx(file)

  clist = sort(unique(c(mat1$chosen_class,mat2$chosen_class,mat3$chosen_class)))
  nlist = c("class","MCF7","U2OS","HepaRG","max")
  res = as.data.frame(matrix(nrow=length(clist),ncol=length(nlist)))
  names(res) = nlist
  for(i in 1:length(clist)) {
    cclass = clist[i]
    x1 = mat1[mat1$chosen_class==cclass,"nhit"]
    x2 = mat2[mat2$chosen_class==cclass,"nhit"]
    x3 = mat3[mat3$chosen_class==cclass,"nhit"]
    if(length(x1)==0) x1 = NA
    if(length(x2)==0) x2 = NA
    if(length(x3)==0) x3 = NA

    m1 = NA
    m2 = NA
    m3 = NA
    if(length(x1)>=nmin) m1 = median(x1,na.rm=T)
    if(length(x2)>=nmin) m2 = median(x2,na.rm=T)
    if(length(x3)>=nmin) m3 = median(x3,na.rm=T)

    cat(cclass,m1,m2,m3,"\n")
    res[i,"class"] = cclass
    res[i,"MCF7"] = m1
    res[i,"U2OS"] = m2
    res[i,"HepaRG"] = m3
    res[i,"max"] = max(m1,m2,m3,na.rm=T)
  }
  res = res[order(res$max,decreasing = TRUE),]
  res = res[res$max>=5,]

  rownames(res) = res$class
  res = as.matrix(res[,c("MCF7","U2OS","HepaRG")])
  rs = rowSums(res)
  res = res[!is.na(rs),]
  #res = log10(res+1)
  res = res[1:40,]
  res[res>color.cut] = color.cut
  #my_palette <- colorRampPalette(c("white", "yellow", "red"))(n = 10)
  heatmap.2(res,Colv=F,Rowv=T,trace="none",margins=c(10,20),cexRow=0.7,cexCol=1,dendrogram="row",
            col=brewer.pal(n = 9, "Reds"))
  # heatmap.2(res,Colv=F,Rowv=F,trace="none",dendrogram = "none",margins=c(10,30),
  #           col=my_palette,colsep=c(1:ncol(res)),rowsep=c(1:nrow(res)),sepcolor="gray",sepwidth =c(0.01,0.01))
  if(!to.file) browser()
  else dev.off()


}

