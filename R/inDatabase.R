#' inDatabase Function to check if something is in the database
#'
#' @import RPostgreSQL
#' @param gid A gid
#' @param dbenv environment with connection to datbase
#' @return a boolean indicating if it is in the database or not
#' @export
#'
inDatabase<- function(gid, dbenv){
  assign('gid', gid, envir=dbenv)
  if(is.na(gid)){
    tf<- FALSE
  }else{
    rec<- with(dbenv, dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",gid)))
    tf<- nrow(rec)>0
  }
  return(tf)
}
