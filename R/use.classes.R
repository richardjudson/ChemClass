#'
#' Add the main use classes
#'
use.classes <- function() {
  printCurrentFunction()
  dir = "data/input/"

  file = "data/input/pesticide classes master.xlsx"
  print(file)
  pest0 = read.xlsx(file)

  file = paste0(dir,"drug classes master.xlsx")
  print(file)
  drugs2 = read.xlsx(file)

  file = paste0(dir,"expert classes master.xlsx")
  print(file)
  expert = read.xlsx(file)

  x = drugs2[is.element(drugs2$dtxsid,pest0$dtxsid),]
  if(nrow(x)>0) {
    cat("overlaps between drug and pesticide\n")
    browser()
  }

  x = drugs2[is.element(drugs2$dtxsid,expert$dtxsid),]
  if(nrow(x)>0) {
    cat("overlaps between drug and expert\n")
    browser()
  }

  x = pest0[is.element(pest0$dtxsid,expert$dtxsid),]
  if(nrow(x)>0) {
    cat("overlaps between pesticide and expert\n")
    browser()
  }

  #-------------------------------------------------------------------
  # Add the thyroid chemicals to the dataframe
  #-------------------------------------------------------------------
  file = paste0(dir,"DIO-IYD_testlib_v3.5_classified_082323 RSJ.xlsx")
  print(file)
  thyroid = read.xlsx(file)
  thyroid = thyroid[,c("dsstox_substance_id","name")]
  names(thyroid)[1] = "dtxsid"
  dlist.thyroid = thyroid$dtxsid
  res = thyroid
  cat("thyroid",nrow(res),length(unique(res$dtxsid)),"\n")

  #-------------------------------------------------------------------
  # Add the zebrafish chemicals to the dataframe
  #-------------------------------------------------------------------
  file = paste0(dir,"zf devtox phase 123.xlsx")
  print(file)
  zf = read.xlsx(file)
  zf = unique(zf[,c("dtxsid","name")])
  dlist.zf = zf$dtxsid
  zf = zf[!is.element(zf$dtxsid,res$dtxsid),]
  res = rbind(res,zf)
  cat("zf",nrow(res),length(unique(res$dtxsid)),"\n")

  #-------------------------------------------------------------------
  # Add the biosolids chemicals to the dataframe
  #-------------------------------------------------------------------
  file = paste0(dir,"Biosolids chemicals.xlsx")
  print(file)
  biosolids = read.xlsx(file)
  biosolids = unique(biosolids[,c("dtxsid","name")])
  dlist.biosolids = biosolids$dtxsid
  biosolids = biosolids[!is.element(biosolids$dtxsid,res$dtxsid),]
  res = rbind(res,biosolids)
  cat("biosolids",nrow(res),length(unique(res$dtxsid)),"\n")

  #-------------------------------------------------------------------
  # Add the toxval chemicals in the TSCA QSAR model to the dataframe
  #-------------------------------------------------------------------
  file = paste0(dir,"toxval pods chemical level oral mgkgday.xlsx")
  print(file)
  toxval = read.xlsx(file)
  toxval = unique(toxval[,c("dtxsid","name")])
  dlist.tsca = toxval$dtxsid
  toxval = toxval[!is.element(toxval$dtxsid,res$dtxsid),]
  res = rbind(res,toxval)
  cat("toxval",nrow(res),length(unique(res$dtxsid)),"\n")

  #-------------------------------------------------------------------
  # Add the oasis chemicals  to the dataframe
  #-------------------------------------------------------------------
  file = paste0(dir,"OASIS chemicals.xlsx")
  print(file)
  oasis = read.xlsx(file)
  oasis = unique(oasis[,c("dtxsid","name")])
  dlist.oasis = oasis$dtxsid
  oasis = oasis[!is.element(oasis$dtxsid,res$dtxsid),]
  res = rbind(res,oasis)
  cat("oasis",nrow(res),length(unique(res$dtxsid)),"\n")

  #-------------------------------------------------------------------
  # Add the httr chemicals to the dataframe
  #-------------------------------------------------------------------
  file = "data/input/HTTr chemicals.xlsx"
  print(file)
  httr0 = read.xlsx(file)
  dlist.httr = httr0$dtxsid
  httr = unique(httr0[!is.element(httr0$dtxsid,res$dtxsid),c("dtxsid","name")])
  res = rbind(res,httr)
  cat("httr",nrow(res),length(unique(res$dtxsid)),"\n")

  #-------------------------------------------------------------------
  # Add the missing toxval chemicals to the dataframe
  #-------------------------------------------------------------------
  file = paste0(dir,"missing class.xlsx")
  print(file)
  missing = read.xlsx(file)
  missing = missing[!is.element(missing$dtxsid,res$dtxsid),]

  dlist = missing$dtxsid
  dups = dlist[duplicated(dlist)]
  x1 = missing[!is.element(missing$dtxsid,dups),]
  x2 = missing[is.element(missing$dtxsid,dups),]
  x2$name = NA
  x2 = unique(x2)
  for(i in 1:nrow(x2)) {
    dtxsid = x2[i,"dtxsid"]
    x2[i,"name"] = missing[missing$dtxsid==dtxsid,"name"][1]
  }
  missing = rbind(x1,x2)

  res = rbind(res,missing)
  cat("missing",nrow(res),length(unique(res$dtxsid)),"\n")

  #-------------------------------------------------------------------
  # Add the classified pesticide chemicals to the dataframe
  #-------------------------------------------------------------------
  file = "data/input/pesticide classes master.xlsx"
  print(file)
  pest0 = read.xlsx(file)
  pest0 = pest0[!is.na(pest0$pesticide_class),]
  dlist.pest = pest0$dtxsid
  pest = pest0[,c("dtxsid","name")]
  pest = unique(pest[!is.element(pest$dtxsid,res$dtxsid),])
  if(nrow(pest)>0) res = rbind(res,pest)
  cat("pest",nrow(res),length(unique(res$dtxsid)),"\n")

  #-------------------------------------------------------------------
  # Add the OPP pesticide chemicals to the dataframe
  #-------------------------------------------------------------------
  file = paste0(dir,"AD Chems for EDSP.xlsx")
  print(file)
  opppest = read.xlsx(file)
  opppest = opppest[,c("DTXSID","Compound.Name")]
  names(opppest) = c("dtxsid","name")
  opppest$class_type = NA
  opppest$chosen_class = NA
  dlist.pest = unique(c(dlist.pest,opppest$dtxsid))


  #-------------------------------------------------------------------
  # Add the classyfire classifications
  #-------------------------------------------------------------------
  res$class_type = NA
  res$chosen_class = NA
  if(!exists("ClassyFire")) {
    file = paste0(dir,"classyfire/ClassyFire DSSTox.RData")
    print(file)
    load(file=file)
    ClassyFire <<- ClassyFire
    rownames(ClassyFire) = ClassyFire$dtxsid
  }
  clist = ClassyFire$dtxsid
  res1 = res[is.element(res$dtxsid,clist),]
  res1 = res1[order(res1$dtxsid),]
  res2 = res[!is.element(res$dtxsid,clist),]
  res = rbind(res1,res2)
  c1 = ClassyFire[res1$dtxsid,c("dtxsid","kingdom","superclass","class","subclass")]
  c1 = c1[order(res1$dtxsid),]
  c2 = as.data.frame(matrix(nrow=nrow(res2),ncol=ncol(c1)))
  names(c2) = names(c1)
  c12= rbind(c1,c2)
  res = cbind(res,c12)

  file = paste0(dir,"classyfire extra chemicals.xlsx")
  print(file)
  cf2 = read.xlsx(file)
  cf2 = unique(cf2)

  # extra = cf2[!is.element(cf2$dtxsid,ClassyFire$dtxsid),]
  # extra = extra[!is.na(extra$kingdom),]
  # browser()
  # file = paste0(dir,"ClassyFire classes not in new set.xlsx")
  # write.xlsx(extra,file)

  for(i in 1:nrow(res)) {
    if(is.na(res[i,"kingdom"])) {
      dtxsid = res[i,"dtxsid"]
      if(is.element(dtxsid,cf2$dtxsid)) {
        x = cf2[cf2$dtxsid==dtxsid,]
        res[i,"kingdom"] = x[1,"kingdom"]
        res[i,"superclass"] = x[1,"superclass"]
        res[i,"class"] = x[1,"class"]
        res[i,"subclass"] = x[1,"subclass"]
      }
    }
  }

  #-------------------------------------------------------------------
  # Add extra columns to the dataframe
  #-------------------------------------------------------------------
  res$thyroid = NA
  res$tsca = NA
  res$pesticide = NA
  res$httr = NA
  res[is.element(res$dtxsid,dlist.thyroid),"thyroid"] = 1
  res[is.element(res$dtxsid,dlist.tsca),"tsca"] = 1
  res[is.element(res$dtxsid,dlist.httr),"httr"] = 1
  res[is.element(res$dtxsid,dlist.pest),"pesticide"] = 1

  res$drugbank = NA
  res$drugbankcat = NA
  res$fdadrug = NA
  res$pfas = NA
  res$pfascat = NA
  res$drugs = NA
  res$httr_use_class = NA
  res$color = NA
  res$metal = NA
  res = unique(res)
  res = res[!is.na(res$name),]
  res[res$dtxsid=="DTXSID2034702","name"] = "MCPB-sodium"
  res = unique(res)

  rownames(res) = res$dtxsid

  #-------------------------------------------------------------------
  # Add the httr use class
  #-------------------------------------------------------------------
  for(i in 1:nrow(httr0)) {
    dtxsid = httr0[i,"dtxsid"]
    uc = httr0[i,"use_class"]
    res[dtxsid,"httr_use_class"] = uc
  }

  #-------------------------------------------------------------------
  # Add the PFAS categories
  #-------------------------------------------------------------------
  file = paste0(dir,"pfas_catalog 2023-07-05.xlsx")
  print(file)
  pfas = read.xlsx(file)
  rownames(pfas) = pfas$dtxsid
  res[is.element(res$dtxsid,pfas$dtxsid),"pfas"] = 1
  pfas = pfas[is.element(pfas$dtxsid,res$dtxsid),]

  #-------------------------------------------------------------------
  # Add the drugbank information
  #-------------------------------------------------------------------
  file = paste0(dir,"drugs/drugbank chemicals.xlsx")
  print(file)
  drugbank = read.xlsx(file)
  res[is.element(res$dtxsid,drugbank$dtxsid),"drugbank"] = 1

  file = paste0(dir,"drugs/drugbank category data.xlsx")
  print(file)
  dbcat = read.xlsx(file)
  for(i in 1:nrow(dbcat)) {
    bn = dbcat[i,"brand_name"]
    gn = dbcat[i,"generic_name"]
    dcat = dbcat[i,"drug_category"]
    if(is.element(bn,res$name)) res[res$name==bn,"drugbankcat"] = dcat
    if(is.element(gn,res$name)) res[res$name==gn,"drugbankcat"] = dcat
  }
  res[!is.na(res$drugbankcat),"drugbank"] = 1

  #-------------------------------------------------------------------
  # Add the FDA drugbank information information
  #-------------------------------------------------------------------
  file = paste0(dir,"drugs/drug products.xlsx")
  print(file)
  fda = read.xlsx(file)
  nlist = unique(c(fda$ActiveIngredient,fda$DrugName))
  nlist = tolower(nlist)
  res[is.element(tolower(res$name),nlist),"fdadrug"] = 1

  #-------------------------------------------------------------------------
  # add the pesticide class information
  #-------------------------------------------------------------------------
  for(i in 1:nrow(pest0)) {
    dtxsid = pest0[i,"dtxsid"]
    if(pest0[i,"pesticide_class"]=="fungicide") pest0[i,"pesticide_class"] = "Antifungal"
    vals = c(pest0[i,"pesticide_class"],pest0[i,"structure_class"],pest0[i,"moa_class"])
    vals = vals[!is.na(vals)]
    label = paste(vals,collapse=" ")
    #label = paste0(pest0[i,"pesticide_class"]," ",pest0[i,"structure_class"]," ",pest0[i,"moa_class"])
    #if(is.na(pest0[i,"structure_class"])) label = pest0[i,"pesticide_class"]
    res[dtxsid,"chosen_class"] = label
    res[dtxsid,"class_type"] = "Pesticide"
    res[dtxsid,"pesticide"] = "1"
  }


  #-------------------------------------------------------------------------
  # add the color
  #-------------------------------------------------------------------------
  file = paste0(dir,"colored chemicals.xlsx")
  print(file)
  colors = read.xlsx(file)
  colors = colors[is.element(colors$dtxsid,res$dtxsid),]
  for(i in 1:nrow(colors)) {
    dtxsid = colors[i,"dtxsid"]
    color = colors[i,"color"]
    res[dtxsid,"color"] = color
  }

  #-------------------------------------------------------------------------
  # add the metals
  #-------------------------------------------------------------------------
  # file = paste0(dir,"metals.xlsx")
  # print(file)
  # metals = read.xlsx(file)
  # metals = metals[is.element(metals$dtxsid,res$dtxsid),]
  # res[is.element(res$dtxsid,metals$dtxsid),"metal"] = 1
  # for(i in 1:nrow(metals)) {
  #   dtxsid = metals[i,"dtxsid"]
  #   metal = metals[i,"metal"]
  #   class = res[dtxsid,"class_type"]
  #   name = res[dtxsid,"name"]
  #   res[dtxsid,"class_type"] = "Organometallic"
  #   res[dtxsid,"chosen_class"] = paste("Organometallic",metal)
  # }

  #-------------------------------------------------------------------------
  # add the expert lists
  #-------------------------------------------------------------------------
  file = paste0(dir,"expert classes master.xlsx")
  print(file)
  expert = read.xlsx(file)
  expert = expert[is.element(expert$dtxsid,res$dtxsid),]
  for(i in 1:nrow(expert)) {
    dtxsid = expert[i,"dtxsid"]
    class = expert[i,"chosen_class"]
    res[dtxsid,"class_type"] = "Expert"
    res[dtxsid,"chosen_class"] = class
  }

  #-------------------------------------------------------------------------
  # add the pfas information
  #-------------------------------------------------------------------------
  for(i in 1:nrow(pfas)) {
    dtxsid = pfas[i,"dtxsid"]
    class = res[dtxsid,"chosen_class"]
    res[dtxsid,"class_type"] = "PFAS"
    res[dtxsid,"chosen_class"] = pfas[i,"category"]
  }

  #-------------------------------------------------------------------------
  # add the drug information
  #-------------------------------------------------------------------------
  file = paste0(dir,"drug classes master.xlsx")
  print(file)
  drugs2 = read.xlsx(file)
  drugs2 = drugs2[is.element(drugs2$dtxsid,res$dtxsid),]
  res[is.element(res$dtxsid,drugs2$dtxsid),"drugs2"] = 1
  for(i in 1:nrow(drugs2)) {
    dtxsid = drugs2[i,"dtxsid"]
    dc = drugs2[i,"drug_class"]
    res[dtxsid,"chosen_class"] = dc
    res[dtxsid,"class_type"] = "Drug"
  }

  #-------------------------------------------------------------------------
  # add the classyfire classes
  #-------------------------------------------------------------------------
  for(i in 1:nrow(res)) {
    dtxsid = res[i,"dtxsid"]
    if(is.na(res[i,"class_type"])) {
      if(!is.na(res[i,"subclass"])) res[i,"chosen_class"] = res[i,"subclass"]
      else if(!is.na(res[i,"class"])) res[i,"chosen_class"] = res[i,"class"]
      else if(!is.na(res[i,"superclass"])) res[i,"chosen_class"] = res[i,"superclass"]
      else if(!is.na(res[i,"kingdom"])) res[i,"chosen_class"] = res[i,"kingdom"]
      if(!is.na(res[i,"chosen_class"]))  res[i,"class_type"] = "ClassyFire"
    }
  }
  res[is.na(res$class_type),"class_type"] = "Unclassed"

  #-------------------------------------------------------------------------
  # read the colors to the unclassed chemicals
  #-------------------------------------------------------------------------
  for(i in 1:nrow(res)) {
    dtxsid = res[i,"dtxsid"]
    if(res[i,"class_type"]=="Unclassed" && !is.na(res[i,"color"])) {
      res[i,"class_type"] = "Color"
      res[i,"chosen_class"] = paste("Color",res[i,"color"])
    }
  }

  res[is.na(res$chosen_class),"chosen_class"] = "Unclassed"
  #-------------------------------------------------------------------------
  # clean up and export
  #-------------------------------------------------------------------------
  res[is.na(res$class_type),"class_type"] = "Unclassed"
  res$chosen_class = str_trim(res$chosen_class)
  res[is.na(res$chosen_class),"chosen_class"] = "Unclassed"
  res[res$chosen_class=="Unclassified","chosen_class"] = "Unclassed"
  res[res$chosen_class=="unclassified","chosen_class"] = "Unclassed"
  res[res$chosen_class=="Unclassed","chosen_class"] = "Unclassified"

  file = "data/input/chemclass level 2.xlsx"
  write.xlsx(res,file)
}
