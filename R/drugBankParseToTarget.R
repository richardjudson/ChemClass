

#--------------------------------------------------------------------------------------
#
# parse the drugbank data
#
#--------------------------------------------------------------------------------------
drugBankParseToTarget <- function(do.load=T) {
  if(do.load) {
    file <- "../drugbank/drugbank.txt"
    data <- readLines(file);
    DRUGBANK.0 <<- data
  }  
  name.list <- c("drugcard_id","name","value")
  row <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) <- name.list
  row[] <- NA
  mat <- NULL
  restart <- 0
  pointer <- 0
  ntot <- length(DRUGBANK.0)
  started <- 0
  counter <- 1
  
  dbid <- ""
  
  while(counter<ntot) {
    line <- DRUGBANK.0[counter]
    #print(line)
    if(nchar(line)>0) {
      temp <- stri_split_fixed(line," ")
      key <- temp[[1]][1]
      val <- temp[[1]][2]
      if(key=="#BEGIN_DRUGCARD") {
        print(temp[[1]])
        dbid <- val
        #print(mat)
        #browser()
        counter <- counter+1
      }
      else if(key=="#" && val=="CAS_Registry_Number:") {
        counter <- counter+1
        casrn <- DRUGBANK.0[counter]
        code <- paste("C",casrn,sep="")
        code <- str_replace_all(code,"-","")
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "casrn"; row[1,"value"] <- casrn; mat <- rbind(mat,row)
        row[1,"name"] <- "code"; row[1,"value"] <- code; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Generic_Name:") {
        counter <- counter+1
        cname <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "generic_name"; row[1,"value"] <- cname; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Brand_Names:") {
        counter <- counter+1
        cname <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "brand_name"; row[1,"value"] <- cname; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="InChI_Key:") {
        counter <- counter+1
        inchi_key <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "inchi_key"; row[1,"value"] <- inchi_key; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Description:") {
        counter <- counter+1
        description <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "description"; row[1,"value"] <- description; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Pharmacology:") {
        counter <- counter+1
        pharmacology <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "pharmacology"; row[1,"value"] <- pharmacology; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Mechanism_Of_Action:") {
        counter <- counter+1
        moa <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "moa"; row[1,"value"] <- moa; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Category:") {
        counter <- counter+1
        drug_category <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "drug_category"; row[1,"value"] <- drug_category; mat <- rbind(mat,row)
        counter <- counter+1
      }
      #
      # the gene symbols
      #
      else if(key=="#" && val=="Drug_Target_1_Gene_Name:") {
        counter <- counter+1
        gene_name_1 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "gene_name_1"; row[1,"value"] <- gene_name_1; mat <- rbind(mat,row)
        #row[1,"name"] <- "gene_mode_1"; row[1,"value"] <- "unspecified"; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_2_Gene_Name:") {
        counter <- counter+1
        gene_name_2 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "gene_name_2"; row[1,"value"] <- gene_name_2; mat <- rbind(mat,row)
        #row[1,"name"] <- "gene_mode_2"; row[1,"value"] <- "unspecified"; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_3_Gene_Name:") {
        counter <- counter+1
        gene_name_3 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "gene_name_3"; row[1,"value"] <- gene_name_3; mat <- rbind(mat,row)
        #row[1,"name"] <- "gene_mode_3"; row[1,"value"] <- "unspecified"; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_4_Gene_Name:") {
        counter <- counter+1
        gene_name_4 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "gene_name_4"; row[1,"value"] <- gene_name_4; mat <- rbind(mat,row)
        #row[1,"name"] <- "gene_mode_4"; row[1,"value"] <- "unspecified"; mat <- rbind(mat,row)
        counter <- counter+1
      }
      #
      # the gene ids
      #
      else if(key=="#" && val=="Drug_Target_1_GenBank_ID_Gene:") {
        counter <- counter+1
        gene_id_1 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "gene_id_1"; row[1,"value"] <- gene_id_1; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_2_GenBank_ID_Gene:") {
        counter <- counter+1
        gene_id_2 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "gene_id_2"; row[1,"value"] <- gene_id_2; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_3_GenBank_ID_Gene:") {
        counter <- counter+1
        gene_id_3 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "gene_id_3"; row[1,"value"] <- gene_id_3; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_4_GenBank_ID_Gene:") {
        counter <- counter+1
        gene_id_4 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "gene_id_4"; row[1,"value"] <- gene_id_4; mat <- rbind(mat,row)
        counter <- counter+1
      }
      #
      # the HGNC gene ids
      #
      else if(key=="#" && val=="Drug_Target_1_HGNC_ID:") {
        counter <- counter+1
        gene_id_1 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "hgnc_gene_id_1"; row[1,"value"] <- gene_id_1; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_2_HGNC_ID:") {
        counter <- counter+1
        gene_id_2 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "hgnc_gene_id_2"; row[1,"value"] <- gene_id_2; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_3_HGNC_ID:") {
        counter <- counter+1
        gene_id_3 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "hgnc_gene_id_3"; row[1,"value"] <- gene_id_3; mat <- rbind(mat,row)
        counter <- counter+1
      }
      else if(key=="#" && val=="Drug_Target_4_HGNC_ID:") {
        counter <- counter+1
        gene_id_4 <- DRUGBANK.0[counter]
        row[1,"drugcard_id"] <- dbid
        row[1,"name"] <- "hgnc_gene_id_4"; row[1,"value"] <- gene_id_4; mat <- rbind(mat,row)
        counter <- counter+1
      }
      
      #
      # the gene references
      #
      else if(key=="#" && val=="Drug_Target_1_Drug_References:") {
        empty <- F
        while(!empty) {
          counter <- counter+1
          line <- DRUGBANK.0[counter]
          if(nchar(line)>0) {row[1,"drugcard_id"] <- dbid; row[1,"name"] <- "gene_ref_1"; row[1,"value"] <- line; mat <- rbind(mat,row)}
          else empty <- T
        }
      }
      else if(key=="#" && val=="Drug_Target_2_Drug_References:") {
        empty <- F
        while(!empty) {
          counter <- counter+1
          line <- DRUGBANK.0[counter]
          if(nchar(line)>0) {row[1,"drugcard_id"] <- dbid; row[1,"name"] <- "gene_ref_2"; row[1,"value"] <- line; mat <- rbind(mat,row)}
          else empty <- T
        }
      }
      else if(key=="#" && val=="Drug_Target_3_Drug_References:") {
        empty <- F
        while(!empty) {
          counter <- counter+1
          line <- DRUGBANK.0[counter]
          if(nchar(line)>0) {row[1,"drugcard_id"] <- dbid; row[1,"name"] <- "gene_ref_3"; row[1,"value"] <- line; mat <- rbind(mat,row)}
          else empty <- T
        }
      }
      else if(key=="#" && val=="Drug_Target_4_Drug_References:") {
        empty <- F
        while(!empty) {
          counter <- counter+1
          line <- DRUGBANK.0[counter]
          if(nchar(line)>0) {row[1,"drugcard_id"] <- dbid; row[1,"name"] <- "gene_ref_4"; row[1,"value"] <- line; mat <- rbind(mat,row)}
          else empty <- T
        }
      }
      #
      # the drug category
      #
      else if(key=="#" && val=="Drug_Category:") {
        empty <- F
        while(!empty) {
          counter <- counter+1
          line <- DRUGBANK.0[counter]
          #cat(nchar(line),line,"\n")
          if(nchar(line)>0) {row[1,"drugcard_id"] <- dbid; row[1,"name"] <- "drug_category"; row[1,"value"] <- line; mat <- rbind(mat,row)}
          else break;
        }
      }
      
      
      
      
      else counter <- counter+1
      
    }
    else counter <- counter+1
  }
  #print(mat)
  file <- "../drugbank/drugbank_index_1_2018-06-12.xlsx"
  write.xlsx(mat,file)
}