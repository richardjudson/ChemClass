library(digest)
#-----------------------------------------------------------------------------------
#' Build a data frame of the data for the toxval manuscript
#'
#' @param toxval.db Database version
#' @param source The source to be updated
#' @return Write a file with the results
#'
#-----------------------------------------------------------------------------------
export.for.toxvaldb <- function(toxval.db,source=NULL) {
  printCurrentFunction(toxval.db)
  cat("Don't forget to run setDBConn() before exporting data\n")
  dir = "data/input/"
  slist = runQuery("select distinct source from toxval",toxval.db)[,1]
  res = NULL
  if(!is.null(source)) slist = source

  #slist = slist[!is.element(slist,c("ECOTOX"))]
  for(src in slist) {
    n = runQuery(paste0("select count(*) from toxval where source='",src,"'"),toxval.db)[1,1]
    cat(src,":",n,"\n")
    query = paste0("SELECT
                    a.dtxsid,a.casrn,a.name,
                    b.source,b.subsource,
                    b.qc_status,
                    b.human_eco,
                    b.toxval_type,
                    b.toxval_subtype,
                    e.toxval_type_supercategory,
                    b.toxval_numeric_qualifier,
                    b.toxval_numeric,
                    b.toxval_units,
                    b.mw,
                    b.toxval_numeric_original,
                    b.toxval_units_original,
                    b.risk_assessment_class,
                    b.study_type,
                    b.study_type_original,
                    b.study_duration_value,
                    b.study_duration_units,
                    b.study_duration_class,
                    d.species_id,d.common_name,d.latin_name,d.ecotox_group,
                    b.strain,
                    b.strain_group,
                    b.sex,
                    b.generation,
                    b.exposure_route,
                    b.exposure_method,
                    b.critical_effect,
                    b.critical_effect_original,
                    b.year,
                    f.long_ref,
                    f.title,
                    f.author,
                    f.journal,
                    f.volume,
                    f.year as ref_year,
                    f.issue,
                    f.url,
                    b.study_group,
                    b.source_hash,
                    a.cleaned_casrn,a.cleaned_name
                    FROM
                    toxval b
                    INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                    LEFT JOIN species d on b.species_id=d.species_id
                    INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                    INNER JOIN record_source f on b.toxval_id=f.toxval_id
                    WHERE
                    b.source='",src,"'
                    and human_eco='human health'
                    and toxval_type_supercategory in ('Point of Departure','Lethality Effect Level','Toxicity Value')
                    limit 10
                   ")
    query = paste0("SELECT
                    a.dtxsid,a.casrn,a.name,
                    b.source,b.subsource,
                    a.cleaned_casrn,a.cleaned_name
                    FROM
                    toxval b
                    INNER JOIN source_chemical a on a.chemical_id=b.chemical_id
                    INNER JOIN toxval_type_dictionary e on b.toxval_type=e.toxval_type
                    WHERE
                    b.source='",src,"'
                    and b.human_eco='human health'
                    and e.toxval_type_supercategory in ('Point of Departure','Lethality Effect Level','Toxicity Value')
                   ")
    mat = runQuery(query,toxval.db,T,F)
    mat = unique(mat)
    if(nrow(mat)>0) {
      mat[is.na(mat$casrn),"casrn"] = mat[is.na(mat$casrn),"cleaned_casrn"]
      mat[mat$casrn=='-',"casrn"] = mat[mat$casrn=='-',"cleaned_casrn"]
      mat[is.na(mat$name),"name"] = mat[is.na(mat$name),"cleaned_name"]
      mat[mat$name=='-',"name"] = mat[mat$name=='-',"cleaned_name"]
      cat(source,nrow(mat),"\n")
      cremove = c("cleaned_name","cleaned_casrn")
      mat = mat[ , !(names(mat) %in% cremove)]
      if(nrow(mat)>0) {
        res = rbind(res,mat)
        cat("   ",nrow(mat),nrow(res),"\n")
      }
    }
  }

  sty = createStyle(halign="center",valign="center",textRotation=90,textDecoration = "bold")
  file = paste0(dir,"/toxval chemicals.xlsx")
  openxlsx::write.xlsx(res,file,firstRow=T,headerStyle=sty)
}
