#' getF1 function to get gid of the nearest f1 cross
#'
#' @import RPostgreSQL
#' @param gid a gid of interest
#' @param dbenv the database connection environment
#' @return A sparse relationship matrix
#' @export
#'
getF1<- function(gid, dbenv){
  assign('gid', gid, envir=dbenv)
  if(is.na(gid)){
    f1<- NA
  }else{
    germplasm <- with(dbenv,dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",gid)))
    tf<- nrow(germplasm)>0
    if(tf){
      if(germplasm[,'gnpgs']==-1){
        f1 <- germplasm[1,"gpid1"]
      }
      if(germplasm[,'gnpgs']==2 | germplasm[,'gnpgs']==1){
        f1 <- gid
      }
      if(germplasm[,'gnpgs']==0){
        f1<- gid
      }
      if(f1==0){
        f1<- NA
      }
    }else{
      f1<- NA
    }
  }
  return(f1)
}
