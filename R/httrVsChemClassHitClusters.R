library(reshape2)
#--------------------------------------------------------------------------------------
#'
#' Cluster chemicals by their hit patterns
#'
#--------------------------------------------------------------------------------------
httrVsChemClassHitClusters <- function(dataset="MCF7 Screen") {
  printCurrentFunction(dataset)
  dir = paste0("data/input/")

  cat("read the catalog\n")
  if(!exists("CATALOG")) {
    file = paste0(dir,"httr/signatureDB_master_catalog merged 2023-08-01.xlsx")
    catalog = read.xlsx(file)
    CATALOG <<- catalog
  }

  cat("read the httr data\n")
  if(dataset=="MCF7 Screen") {
    if(!exists("MCF7")) {
      file = paste0(dir,"httr/SIGNATURE_CR_screen_large_mcf7_ph1_pe1_normal_block_123_allPG_gsea_0.05_conthits.RData")
      load(file=file)
      httr = SIGNATURE_CR
      catalog = CATALOG[is.element(CATALOG$parent,httr$signature),]
      catalog = catalog[!is.na(catalog$gene_target),]
      catalog = unique(catalog[,c("parent","super_target","gene_target")])
      names(catalog)[1] = "signature"
      httr = httr[is.element(httr$signature,catalog$signature),]
      httr2 = merge(httr,catalog,by="signature")
      names(httr2)[39] = "super_target"
      h1 = unique(httr2[,c("signature","super_target","super_target.y","gene_target")])
      slist = h1$signature
      dups = slist[duplicated(slist)]
      httr3 = httr2[!is.element(httr2$signature,dups),]
      MCF7 <<- httr3
    }
  }
  if(dataset=="U2OS Screen") {
    if(!exists("U2OS")) {
      file = paste0(dir,"httr/SIGNATURE_CR_screen_large_u2os_toxcast_pfas_pe1_normal_v2_gsea_0.05_conthits.RData")
      load(file=file)
      httr = SIGNATURE_CR
      catalog = CATALOG[is.element(CATALOG$parent,httr$signature),]
      catalog = catalog[!is.na(catalog$gene_target),]
      catalog = unique(catalog[,c("parent","super_target","gene_target")])
      names(catalog)[1] = "signature"
      httr = httr[is.element(httr$signature,catalog$signature),]
      httr2 = merge(httr,catalog,by="signature")
      names(httr2)[39] = "super_target"
      h1 = unique(httr2[,c("signature","super_target","super_target.y","gene_target")])
      slist = h1$signature
      dups = slist[duplicated(slist)]
      httr3 = httr2[!is.element(httr2$signature,dups),]
      U2OS <<- httr3
    }
  }
  if(dataset=="HepaRG Screen") {
    if(!exists("HepaRG")) {
      file = paste0(dir,"httr/SIGNATURE_CR_screen_large_heparg2d_toxcast_pfas_pe1_normal_v2_gsea_0.05_conthits.RData")
      load(file=file)
      httr = SIGNATURE_CR
      catalog = CATALOG[is.element(CATALOG$parent,httr$signature),]
      catalog = catalog[!is.na(catalog$gene_target),]
      catalog = unique(catalog[,c("parent","super_target","gene_target")])
      names(catalog)[1] = "signature"
      httr = httr[is.element(httr$signature,catalog$signature),]
      httr2 = merge(httr,catalog,by="signature")
      names(httr2)[39] = "super_target"
      h1 = unique(httr2[,c("signature","super_target","super_target.y","gene_target")])
      slist = h1$signature
      dups = slist[duplicated(slist)]
      httr3 = httr2[!is.element(httr2$signature,dups),]
      HepaRG <<- httr3
    }
  }
  if(dataset=="MCF7 Screen") httr = MCF7
  if(dataset=="U2OS Screen") httr = U2OS
  if(dataset=="HepaRG Screen") httr = HepaRG

  cat("read the chemclass data\n")
  file = paste0(dir,"chemclass level 2.xlsx")
  chemclass = read.xlsx(file)
  rownames(chemclass) = chemclass$dtxsid
  chemclass = chemclass[is.element(chemclass$dtxsid,httr$dtxsid),]
  chems = chemclass[,c("dtxsid","name","chosen_class")]
  chems = chems[order(chems$chosen_class),]
  rownames(chems) = chems$dtxsid

  cat("subset the httr data\n")
  httr = httr[is.element(httr$dtxsid,chems$dtxsid),]
  httr = httr[!is.na(httr$gene_target),]
  httr2 = httr[httr$hitcall>=0.9,]

  cat("get the denominator for the hit rate\n")
  s1 = unique(httr2[,c("signature","gene_target")])
  slist = s1$gene_target
  scounts = as.data.frame(table(slist),stringsAsFactors = FALSE)
  names(scounts) = c("gene_target","n")
  rownames(scounts) = scounts$gene_target
  scounts = scounts[scounts$n>=10,]

  cat("great the count matrix, numerator\n")
  httr2 = httr2[is.element(httr2$gene_target,scounts$gene_target),]
  httr3 = unique(httr2[,c("dtxsid","signature","gene_target")])
  t1 = as.data.frame(table(httr3[,c("dtxsid","gene_target")]),stringsAsFactors = FALSE)
  t2 = t1
  for(gt in scounts$gene_target) {
    t2[t2$gene_target==gt,"Freq"] = t1[t1$gene_target==gt,"Freq"]/scounts[gt,"n"]
    maxval = max(t2[t2$gene_target==gt,"Freq"])
    if(maxval>1) browser()
  }
  t3 = dcast(t2,formula=dtxsid~gene_target)
  rownames(t3) = t3$dtxsid
  t3 = t3[,2:ncol(t3)]
  t3[is.na(t3)] = 0
  hitFractionMatrix = as.data.frame(t3)
  cat("chemicals: ",nrow(hitFractionMatrix),"\n")
  cat("gene targets: ",ncol(hitFractionMatrix),"\n")
  file = paste0(dir,"httr/httrVsChemClassHitClusters matrix ",dataset,".RData")
  save(hitFractionMatrix,file=file)
}
