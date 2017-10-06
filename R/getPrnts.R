#' getPrnts Function to get parents of an individaul
#'
#' @import RPostgreSQL
#' @param gid A numeric gid
#' @param dbenv the database connection environment
#' @param pastenm a boolean indicating if parent gids should be pasted together
#' @return the parent gids
#' @export
#'
getPrnts<- function(gid, dbenv, pastenm=F){
  if(gid==0 | is.na(gid)){
    prnts<- c(0, 0)
  }else{
    f1<- getF1(gid, dbenv) #gid of the cros
    if(is.na(f1) | f1==0){
      prnts<- c(0, 0)
    }else{
      assign('f1', f1, envir=dbenv)
      f1inf<- with(dbenv,dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",f1))) #query of cross
      prnts<- as.numeric(f1inf[1,c(4:5)]) #gids of the parents
    }
  }
  if(pastenm){
    prnts<- paste(prnts, collapse=",")
  }
  return(prnts)
}
