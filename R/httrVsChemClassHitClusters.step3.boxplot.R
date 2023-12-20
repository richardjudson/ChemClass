library(reshape2)
library(vegan)
#--------------------------------------------------------------------------------------
#'
#' Box plot of the nearest chemcial of the same class
#'
#' process to get here
#' httrVsChemClassHitClusters.step3.boxplot
#' httrVsChemClassHitClusters.step3
#' httrVsChemClassHitClusters.step2
#' httrVsChemClassHitClusters
#'
#--------------------------------------------------------------------------------------
httrVsChemClassHitClusters.step3.boxplot <- function(to.file=F,dataset="MCF7 Screen",nmax = 1000) {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")

  file = paste0(dir,"httr/httrVsChemClassHitClusters minmatch ",dataset,".xlsx")
  r2 = read.xlsx(file)
  r3 = r2[,c("chosen_class","minmatch")]
  r3$minmatch = 0
  r3 = unique(r3)
  for(i in 1:nrow(r3)) {
    cc = r3[i,"chosen_class"]
    r3[i,"minmatch"] = median(r2[r2$chosen_class==cc,"minmatch"])
  }
  r3 = r3[r3$minmatch<nrow(r2)/20,]
  r2 = r2[is.element(r2$chosen_class,r3$chosen_class),]
  counts = as.data.frame(table(r2$chosen_class))
  counts = counts[order(counts$Freq),]
  title = paste0(dataset)
  p = ggplot(data=r2,aes(x=reorder(chosen_class,minmatch,FUN=median),y=minmatch))  +
    ggtitle(title) +
    geom_boxplot(outlier.colour="black",
                 outlier.shape=21,outlier.size=2,outlier.fill="white",
                 notch=FALSE,size=0.1) +
    scale_fill_manual(values=c("white","red")) +
    coord_flip() +
    theme_bw() +
    ylab(paste0("Closest Class Match")) +
    xlab("") +
    ylim(c(0,250)) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=7),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
          strip.text.x = element_text(size = 10),
          plot.margin = margin(t=20,r=20,b=50,l=20))# +
  print(p)

  if(to.file) {
    fname = paste0(dir,"httr/httrVsChemClassHitClusters.step3.boxplot ",dataset,".pdf")
    ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()
}
