library(classyfireR)
library(R.utils)
#-------------------------------------------------------------------------------
#' Run the classyfireR service for a series of batches. The input is a file
#' with 2 columns: dtxsid and smiles. Each SMILES is sent to the classyfireR
#' server and a classification is returned if successful.
#' The service is buggy, so several hacks are built into this function to
#' handle the different error conditions of the server. THe most vexing problem
#' is that the server can go into an infinite loop without a return. These are
#' indicated by "Request failed [xxx]" messages. These are mostly solved with
#' a withTimeout condition. These errors seem to be caused by bad SMILES. The
#' Dashboard return a single space when a SMILES is missing and these
#' will cause this error. There other problematic bad SMILES, including ones
#' with a # character (triple bond). Still need to find all bad types and
#' filter them out. Linear alkanes (CCCC...CCC) are not classified
#' and need to be classified by hand.
#'
#' Manual fixes so far:
#' 1. empty strings filtered from initial set and never tried now
#' 2. Triple bonds ("#") replaced with double bond ("="). THis should not
#' affect the classification
#' At the end of each batch of chemicals a file is written out, allowing one to
#' save results along the way and restart if needed. All of these files need to
#' be concatenated together and chemicals with missing classification rerun
#' at least one more time.
#'
#' The output file includes the chemical name as well as the dtxsid. To add
#' this, a file with the DSSTox inventory is read in.
#'
#' @param batchstart The index of the batch of chemicals to run
#' @param batchsize The number of chemicals in a batch
#' @param maxtry The service will sometimes fail but can pass on a later call
#' This parameter is the number of tries to do before giving up
#' @param sleepinterval.batch The number of seconds to wait at the end of a batch.
#' This was an attempt to solve a problem that looked like refusal by the
#' server to handle too many requests in a time period. Probably not needed
#' @param sleepinterval.chemical The number of seconds to wait at the end of
#' a chemical. This was an attempt to solve a problem that looked like refusal
#' by the server to handle too many requests in a time period. Probably not needed
#' @param time.limit ClassyFire R will sometimes go into an infinite loop on
#' the server size without returning. This function will stop waiting for a
#' return after this interval (in seconds) and move to the next chemical
#' @export
#-------------------------------------------------------------------------------
classify <- function(batchstart=1,batchsize=100,maxtry=2,
                     sleepinterval.batch=10,
                     sleepinterval.chemical=5,
                     time.limit=10) {
  printCurrentFunction()
  dir="data/input/"
  file = paste0(dir,"chemclass level 2.xlsx")
  input = read.xlsx(file)
  input = input[is.na(input$kingdom),]
  dlist = input$dtxsid

  file = paste0(dir,"SMILES/all_smiles.xlsx")
  smiles = read.xlsx(file)
  smiles = smiles[smiles$smiles!="N/A",]

  dlist = dlist[is.element(dlist,smiles$dtxsid)]
  smiles.input = smiles[is.element(smiles$dtxsid,dlist),c("dtxsid","name","smiles")]
  cat("SMILES to classify:",nrow(smiles.input),"\n")
  rownames(smiles.input) = smiles.input$dtxsid

  ntot = nrow(smiles.input)
  nbatch = round(ntot / batchsize)
  cat(ntot,nbatch,batchsize,"\n")
  for(batch in batchstart:nbatch) {
    start = 1 + (batch-1)*batchsize
    end = start + batchsize - 1
    if(end>nrow(smiles.input)) end = nrow(smiles.input)
    cat(">>> batch:",batch," ",start,":",end,"\n")
    all.smiles = smiles.input[start:end,]

    res = all.smiles
    res$counter = NA
    res$kingdom = NA
    res$superclass = NA
    res$class = NA
    res$subclass  = NA
    levels = c("kingdom","superclass","class","subclass")
    for(i in 1:nrow(all.smiles)) {
      dtxsid = all.smiles[i,"dtxsid"]
      name = all.smiles[i,"name"]
      smiles = all.smiles[i,"smiles"]
      smiles = str_replace_all(smiles,"#","=")
      if(contains(smiles,"|")) {
        pos = unlist(gregexpr("\\|", smiles))
        smiles = substr(smiles,1,(pos-1))
      }
      smiles = trim(smiles)
      if(contains(smiles,".")) {
        pos = unlist(gregexpr("\\.", smiles))
        a = substr(smiles,1,(pos-1))
        b = substr(smiles,pos+1,nchar(smiles))
        l1 = str_length(a)
        l2 = str_length(b)
        smiles = a
        if(l2>l1) smiles = b
      }
      smiles = trim(smiles)
      if(contains(smiles,".")) {
        pos = unlist(gregexpr("\\.", smiles))
        a = substr(smiles,1,(pos-1))
        b = substr(smiles,pos+1,nchar(smiles))
        l1 = str_length(a)
        l2 = str_length(b)
        smiles = a
        if(l2>l1) smiles = b
      }
      smiles = trim(smiles)
      res[i,"smiles"] = smiles
      res[i,"counter"] = i+start-1
      cat(dtxsid,name,i,nrow(all.smiles),"...",smiles,"\n")
      for(j in 1:maxtry) {
        cat("  try",j,"\n")
        try({
          query = NULL
          withTimeout({
            query = submit_query(label = dtxsid,
                                 input = c(smiles),
                                 type = 'STRUCTURE')
          },
          substitute=F,
          timeout=time.limit,
          elapsed=time.limit,
          onTimeout="warning"
          )
          if(!is.null(query)) {
            x = as.data.frame(classification(query))
            for(level in levels) if(is.element(level,x$Level)) res[i,level] = x[x[,"Level"]==level,"Classification"]
            break()
          }
        })
        if(inherits(query, "try-error")){
          print("skip error")
          next
        }
        else if(inherits(query, "try-warning")){
          print("skip warning")
          #browser()
          next
        }
        Sys.sleep(sleepinterval.chemical)
      }
    }
    browser()
    file = paste0(dir,"classyfire/smiles.classified v2 ",batch,"_",batchsize," ",start," ",end,".xlsx")
    write.xlsx(res,file)
    Sys.sleep(sleepinterval.batch)
  }
}
