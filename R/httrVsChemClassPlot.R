library(ggplot2)
#--------------------------------------------------------------------------------------
#'
#' Plot the HTTr data vs the chemical categories
#'
#--------------------------------------------------------------------------------------
httrVsChemClassPlot <- function(to.file=F,nmin=3,dataset="MCF7 Screen") {
  printCurrentFunction()
  dir = paste0("data/input/httr/")
  file = paste0(dir,"HTTrChemclassStats ",dataset,".xlsx")
  mat = read.xlsx(file)

  cclass = as.data.frame(table(mat$chosen_class))
  good.classes = as.character(cclass[cclass$Freq>=nmin,"Var1"])
  mat2 = mat[is.element(mat$chosen_class,good.classes),]

  #----------------------------------------------------------------------------------------
  title = paste0(dataset," Hits")
  p = ggplot(data=mat2,aes(x=reorder(chosen_class,nhit,FUN=median),y=nhit))  +
    #p = ggplot(data=form,aes(x=Cl,y=pod))  +
    ggtitle(title) +
    geom_boxplot(outlier.colour="black",
                 outlier.shape=21,outlier.size=2,outlier.fill="white",
                 notch=FALSE,size=0.1) +
    #geom_jitter(aes(color=structure_class_source),size=1.5,alpha = 0.9) +
    #geom_hline(yintercept=medpod,color="red",size=2) +
    #scale_y_continuous(trans="log10",limits=c(1e-5,1000)) +
    scale_fill_manual(values=c("white","red")) +
    coord_flip() +
    theme_bw() +
    ylab(paste0("No. Hits")) +
    xlab("") +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=5),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
          strip.text.x = element_text(size = 10),
          plot.margin = margin(t=20,r=20,b=50,l=20))# +
  print(p)

  if(to.file) {
    fname = paste0(dir,"httrVsChemClassPlot ",dataset,"  nhit.pdf")
    ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()

  #----------------------------------------------------------------------------------------
  title = paste0(dataset," POD")
  p = ggplot(data=mat2,aes(x=reorder(chosen_class,pod,FUN=median),y=pod))  +
    #p = ggplot(data=form,aes(x=Cl,y=pod))  +
    ggtitle(title) +
    geom_boxplot(outlier.colour="black",
                 outlier.shape=21,outlier.size=2,outlier.fill="white",
                 notch=FALSE,size=0.1) +
    #geom_jitter(aes(color=structure_class_source),size=1.5,alpha = 0.9) +
    #geom_hline(yintercept=medpod,color="red",size=2) +
    #scale_y_continuous(trans="log10",limits=c(1e-5,1000)) +
    scale_fill_manual(values=c("white","red")) +
    coord_flip() +
    theme_bw() +
    ylab(paste0("log(POD)")) +
    xlab("") +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=5),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
          strip.text.x = element_text(size = 10),
          plot.margin = margin(t=20,r=20,b=50,l=20))# +
  print(p)

  if(to.file) {
    fname = paste0(dir,"httrVsChemClassPlot ",dataset,"  pod .pdf")
    ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()

  #----------------------------------------------------------------------------------------
  title = paste0(dataset," median(BMD)")
  p = ggplot(data=mat2,aes(x=reorder(chosen_class,bmdmed,FUN=median),y=bmdmed))  +
    #p = ggplot(data=form,aes(x=Cl,y=pod))  +
    ggtitle(title) +
    geom_boxplot(outlier.colour="black",
                 outlier.shape=21,outlier.size=2,outlier.fill="white",
                 notch=FALSE,size=0.1) +
    #geom_jitter(aes(color=structure_class_source),size=1.5,alpha = 0.9) +
    #geom_hline(yintercept=medpod,color="red",size=2) +
    #scale_y_continuous(trans="log10",limits=c(1e-5,1000)) +
    scale_fill_manual(values=c("white","red")) +
    coord_flip() +
    theme_bw() +
    ylab(paste0("log(median(BMD))")) +
    xlab("") +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=5),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
          strip.text.x = element_text(size = 10),
          plot.margin = margin(t=20,r=20,b=50,l=20))# +
  print(p)

  if(to.file) {
    fname = paste0(dir,"httrVsChemClassPlot ",dataset,"  bmdmed .pdf")
    ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()
}

