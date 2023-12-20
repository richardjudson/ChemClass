library(reshape2)
#--------------------------------------------------------------------------------------
#'
#' Analyze the HTTr data vs the chemical categories
#' Calculate chisq stats of chemclass vs signature
#'
#--------------------------------------------------------------------------------------
httrVsChemClassChisq <- function(dataset="MCF7 Screen",cutoff=0) {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")

  file = paste0(dir,"chemclass level 2.xlsx")
  chemclass = read.xlsx(file)
  rownames(chemclass) = chemclass$dtxsid

  file = paste0(dir,"httr/signatureDB_master_catalog merged 2023-08-01.xlsx")
  catalog = read.xlsx(file)
  catalog = unique(catalog[,c("parent","gene_target")])
  names(catalog) = c("signature","gene_target")
  x = catalog$signature
  y = x[duplicated(x)]
  if(length(y)>0) browser()
  rownames(catalog) = catalog$signature

  if(!exists("HTTR")) {
    cat("load httr data\n")
    if(dataset=="MCF7 Screen") fname = "SIGNATURE_CR_screen_large_u2os_toxcast_pfas_pe1_normal_v2_gsea_0.05_conthits"
    else if(dataset=="U2OS Screen") fname = "SIGNATURE_CR_screen_large_mcf7_ph1_pe1_normal_block_123_allPG_gsea_0.05_conthits"
    else if(dataset=="HepaRG Screen") fname = "SIGNATURE_CR_screen_large_heparg2d_toxcast_pfas_pe1_normal_v2_gsea_0.05_conthits"
    file = paste0(dir,"httr/",fname,".RData")
    load(file=file)
    HTTR <<- SIGNATURE_CR
  }
  httr = HTTR[,c("dtxsid","signature","super_target","hitcall")]

  dlist = unique(httr$dtxsid)
  x = dlist[!is.element(dlist,chemclass$dtxsid)]
  if(length(x)>0) browser()
  chemclass = chemclass[dlist,]


  chemclass[is.element(chemclass$chosen_class,c("Estrogen flavonoid","Estrogen nonsteroidal","Estrogen antagonist")),"chosen_class"] = "Estrogen"
  slist = unique(httr$signature)
  #slist2 = catalog[catalog$gene_target=="ESR1|ESR2|PGR","signature"]
  #slist = slist[is.element(slist,slist2)]

  clist = unique(chemclass$chosen_class)
  x = as.data.frame(matrix(nrow=length(dlist),ncol=3))
  names(x) = c("dtxsid","chemclass","signature")
  x$dtxsid = dlist
  rownames(x) = dlist
  x$chemclass = 0
  x$signature = 0

  res = NULL
  i = 0

  for(sig in slist) {
    h1 = httr[httr$signature==sig,]
    d1 = h1[h1$hitcall>=0.9,"dtxsid"]
    gt = "None"
    if(is.element(sig,catalog$signature)) gt = catalog[sig,"gene_target"]
    if(length(d1)>=cutoff) {
      for(cc in clist) {
        d2 = chemclass[chemclass$chosen_class==cc,"dtxsid"]
        if(length(d2)>=cutoff) {
          x$chemclass = 0
          x$signature = 0
          x[d1,"signature"] = 1
          x[d2,"chemclass"] = 1
          tp = sum(x$chemclass*x$signature)
          fp = sum(x$chemclass*(1-x$signature))
          fn = sum((1-x$chemclass)*x$signature)
          tn = sum((1-x$chemclass)*(1-x$signature))
          if(tp>=cutoff) {
            y = TxT(tp,fp,fn,tn,do.p=TRUE,chemclass=cc,signature=sig,gene_target=gt)$mat
            res = rbind(res,y)
          }
        }
      }
    }
    i = i+1
    if(i%%100==0) cat("finished ",i," signatures out of ",length(slist),"\n")
  }
  res$padj = NA
  plist = res$p.value
  res$padj = p.adjust(plist,method="fdr")


  file = paste0(dir,"httr/",dataset," chisq.xlsx")
  write.xlsx(res,file)
  browser()
}
