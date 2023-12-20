#-----------------------------------------------------------------------------------
#' Export the relevant chemciasl from toxval
#'
#' @param toxval.db Database version
#' @param source The source to be updated
#' @return Write a file with the results
#'
#-----------------------------------------------------------------------------------
export.toxvaldb.chems <- function(toxval.db="res_toxval_v94") {
  printCurrentFunction(toxval.db)
  dir = "data/input/" # paste0(toxval.config()$datapath,"manuscript_data")

  query = paste0("SELECT
                  a.dtxsid,a.name
                  FROM
                  toxval b
                  INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                  INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                  WHERE
                  b.human_eco='human health'
                  and e.toxval_type_supercategory in ('Point of Departure')")
  chems = runQuery(query,toxval.db,T,F)
  chems = unique(chems)
  cat(nrow(chems),"\n")
  file = paste0(dir,"ToxValDB HH repeat dose chems.xlsx")
  write.xlsx(chems,file)
}
