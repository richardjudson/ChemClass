#-------------------------------------------------------------------------------
#' corrects all smiles list
#'
#' `fix_smiles()` corrects all smiles list
#'
#' @param smile_file The location and file name for the exported excel file created. Null means do not save the file.
#'
#' @export
#-------------------------------------------------------------------------------
fix_smiles <- function(smile_file = "inst/extdata/POD_chemical_SMILES.xlsx") {
  printCurrentFunction()
  dir = "data/output/"
  fnunits = "mgkgday"
  exposure_route = "oral"
  smile_file = paste0(dir,"POD chemical SMILES.xlsx")

  dlist = c("DTXSID4031980",
            "DTXSID20896987",
            "DTXSID3021805",
            "DTXSID601035903",
            "DTXSID9025906",
            "DTXSID9023914"
  )

  #load file
  smile_df <- openxlsx::read.xlsx(smile_file, "Main Data")
  smile_df = smile_df[is.element(smile_df$dtxsid,dlist),]
  browser()
  #remove quotes
  smile_df$QSAR_READY_SMILES <- gsub('"',"", smile_df$QSAR_READY_SMILES)
  browser()
  smile_df$SMILES <- gsub('"',"", smile_df$SMILES) #new code
  browser()

  #change // to /
  #smile_df$QSAR_READY_SMILES <- gsub("//","/", smile_df$QSAR_READY_SMILES)
  #smile_df$SMILES <- gsub("//","/", smile_df$SMILES) #new code

  #change \\ to \
  # smile_df$QSAR_READY_SMILES <- gsub("\\\\","\\", smile_df$QSAR_READY_SMILES)
  #smile_df$SMILES <- gsub("\\\\","\\", smile_df$SMILES) #new code
  browser()
  #removes anything between and including |
  smile_df$QSAR_READY_SMILES <- gsub('\\|.*\\|',"", smile_df$QSAR_READY_SMILES)
  smile_df$SMILES <- gsub('\\|.*\\|',"", smile_df$SMILES) #new code

  #remove space
  smile_df[smile_df == ""] <- NA
  smile_df[smile_df == " "] <- NA

  #correct QSAR_READY_SMILES that contain two smiles.
  #split by comma - a few rows have many columns
  split_smile <- tidyr::separate(data = smile_df, col = QSAR_READY_SMILES, sep = ",", into = c("QSARsmile_part1","QSARsmile_part2"), remove = FALSE)
  split_smile$new_qsar_smile <- NA

  for (i in 1:length(split_smile$QSARsmile_part1)){
    if (is.na(split_smile$QSARsmile_part1[i])){
      split_smile$new_qsar_smile[i] <- NA
    } else if (is.na(split_smile$QSARsmile_part2[i])){
      split_smile$new_qsar_smile[i] <- split_smile$QSARsmile_part1[i]
    } else if (stringr::str_length(split_smile$QSARsmile_part1[i]) >= stringr::str_length(split_smile$QSARsmile_part2[i])){
      split_smile$new_qsar_smile[i] <- split_smile$QSARsmile_part1[i]
    } else if (stringr::str_length(split_smile$QSARsmile_part1[i]) < stringr::str_length(split_smile$QSARsmile_part2[i])){
      split_smile$new_qsar_smile[i] <- split_smile$QSARsmile_part2[i]
    } else
      split_smile$new_qsar_smile[i] <- NA
  }
  browser()
  #########
  #get a sheet of metal chemicals to check if any have qsar smiles (this is separate from fixing the smiles)
  metal_file = "inst/extdata/Metal_List.xlsx"
  metals <- openxlsx::read.xlsx(metal_file, "Sheet 1")
  metal_list = list()
  #filter to include metals
  for (i in metals$Symbol){
    print(i)
    dtxsid_metal <- split_smile[grep(i, split_smile$SMILES, ignore.case=FALSE), ] #search SMILE column for the element
    metal_list[[i]] <- dtxsid_metal # add it to your list
  }
  dtxsid_metal = do.call(rbind, metal_list)
  #Get unique rows
  dtxsid_unique <- unique(dtxsid_metal)
  browser()
  #save
  #openxlsx::write.xlsx(x = dtxsid_unique, file = "data/POD_chemical_SMILES_metals_only.xlsx")
  #There are some that have qsar smiles so need to write code to take the smile instead.
  #########

  #If there is no qsar smile then use the smile if available.
  #New code (not in the original POD_chemical_SMILES_fixed file)
  for (i in 1:length(split_smile$new_qsar_smile)){
    if (is.na(split_smile$new_qsar_smile[i]) & !is.na(split_smile$SMILES[i])){
      split_smile$new_qsar_smile[i] <- split_smile$SMILES[i]
    }
  }
  browser()
  #for smiles that contain metals, use smile instead of qsar smiles even if there is a qsar smile. #New code
  for (i in dtxsid_unique$dtxsid){ #metal dtxsid list
    for (j in 1:length(split_smile$dtxsid)){
      if (split_smile$dtxsid[j] == i){ #check if dtxsid is on the metal list
        split_smile$new_qsar_smile[j] <- split_smile$SMILES[j] #replace with smile
      }
    }
  }
  browser()
  #Save
  #openxlsx::write.xlsx(x = split_smile, file = "data/POD_chemical_SMILES_fixed.xlsx") #this sheet only has fixed qsar smiles
  #openxlsx::write.xlsx(x = split_smile, file = "data/POD_chemical_SMILES_fixed_08032023.xlsx") #fixed qsar smiles and smiles
  openxlsx::write.xlsx(x = split_smile, file = "data/POD_chemical_SMILES_fixed_08182023.xlsx") #fixed metals

  #final file
  split_smile_final <- split_smile[,c("dtxsid","new_qsar_smile")]
  split_smile_final <- dplyr::rename(split_smile_final, "DTXSID" = "dtxsid",  "QSAR_READY_SMILES" = "new_qsar_smile")
  split_smile_final <- split_smile_final[order(split_smile_final$QSAR_READY_SMILES, decreasing = TRUE), ]

  #save
  #openxlsx::write.xlsx(x = split_smile_final, file = "data/POD_chemical_SMILES_fixed_QSAR_Only_08032023.xlsx") #fixed qsar smiles and smiles
  openxlsx::write.xlsx(x = split_smile_final, file = paste0(dir,"POD_chemical_SMILES_fixed_smilecolumn_08182023.xlsx")) #fixed metals
}
