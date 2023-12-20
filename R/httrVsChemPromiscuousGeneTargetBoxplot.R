library(reshape2)
#--------------------------------------------------------------------------------------
#'
#' Analyze the HTTr data vs the chemical categories
#' Run httrVsChemClassHitClusters to generate the input data file
#'
#--------------------------------------------------------------------------------------
httrVsChemPromiscuousGeneTargetBoxplot <- function(to.file=F,dataset="MCF7 Screen") {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")

  file = paste0(dir,"httr/httrVsChemClassHitClusters matrix ",dataset,".RData")
  load(file=file) # gets  hitFractionMatrix
  dlist = rownames(hitFractionMatrix)

  cs = sort(colSums(hitFractionMatrix),decreasing=T)
  hfm = hitFractionMatrix[,names(cs)[1:50]]
  hfm.melt = melt(hfm)
  hfm.melt = hfm.melt[hfm.melt$value>0,]
  title = paste0(dataset)
  p = ggplot(data=hfm.melt,aes(x=reorder(variable,value,FUN=median),y=value))  +
    ggtitle(title) +
    geom_boxplot(outlier.colour="black",
                 outlier.shape=21,outlier.size=2,outlier.fill="white",
                 notch=FALSE,size=0.1) +
    scale_fill_manual(values=c("white","red")) +
    coord_flip() +
    theme_bw() +
    ylab(paste0("Closest Class Match")) +
    xlab("") +
    ylim(c(0,1)) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=7),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
          strip.text.x = element_text(size = 10),
          plot.margin = margin(t=20,r=20,b=50,l=20))# +
  print(p)

  if(to.file) {
    fname = paste0(dir,"httr/httrVsChemPromiscuousGeneTargetBoxplot ",dataset,".pdf")
    ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()

 }
