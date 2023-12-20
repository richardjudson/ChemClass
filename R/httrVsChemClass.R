#--------------------------------------------------------------------------------------
#'
#' Analyze the HTTr data vs the chemical categories
#'
#--------------------------------------------------------------------------------------
httrVsChemClass <- function(dataset) {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")
  if(dataset=="MCF7 Screen") file = paste0(dir,"httr/SIGNATURE_CR_screen_large_mcf7_ph1_pe1_normal_block_123_allPG_gsea_0.05_conthits.RData")
  if(dataset=="HepaRG Screen") file = paste0(dir,"httr/SIGNATURE_CR_screen_large_heparg2d_toxcast_pfas_pe1_normal_v2_gsea_0.05_conthits.RData")
  if(dataset=="U2OS Screen") file = paste0(dir,"httr/SIGNATURE_CR_screen_large_u2os_toxcast_pfas_pe1_normal_v2_gsea_0.05_conthits.RData")
  print(file)
  load(file=file)
  HTTR <<- SIGNATURE_CR

  file = paste0(dir,"chemclass level 2.xlsx")
  chemclass = read.xlsx(file)
  rownames(chemclass) = chemclass$dtxsid
  chemclass = chemclass[is.element(chemclass$dtxsid,HTTR$dtxsid),]

  result = chemclass[,c("dtxsid","name","chosen_class")]
  result$nsig = NA
  result$nhit = NA
  result$pod = NA
  result$bmdmed = NA

  for(i in 1:nrow(result)) {
    dtxsid = result[i,"dtxsid"]
    t1 = HTTR[HTTR$dtxsid==dtxsid,]
    result[i,"nsig"] = nrow(t1)
    t2 = t1[t1$hitcall>0.9,]
    result[i,"nhit"] = nrow(t2)
    t3 = log10(t2$bmd)
    result[i,"bmdmed"] = median(t3)
    x = quantile(t3,probs = seq(0, 1, 0.05))
    result[i,"pod"] = x[2]
    if(i%%100==0) cat("finished",i,"out of ",nrow(result),"\n")
  }
  file = paste0(dir,"httr/HTTrChemclassStats ",dataset,".xlsx")
  write.xlsx(result,file)
}
