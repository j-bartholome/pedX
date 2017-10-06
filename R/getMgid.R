#' getMgid Function for getting the last gid resulting from a derivative method
#'
#' @import RPostgreSQL
#' @param gid A numeric gid
#' @param dbenv the database connection environment
#' @return a numeric gid
#' @export
#'
getMgid<- function(gid, dbenv){
  getMtype<- function(gid, dbenv=dbenv){#Function to get the managament method
  	 assign('gid', gid, envir=dbenv)
    if(!is.na(gid)){
      grm<- with(dbenv, dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",gid)))
      tf<- nrow(grm)>0
      if(tf){
        meth<-grm[1,'methn']
        assign('meth', meth, envir=dbenv)
        mtype<- with(dbenv,dbGetQuery(con, sprintf("SELECT mtype FROM methods WHERE mid=%d",meth)))[,1]
        gpid2<- grm[1,'gpid2']
      }else{
        mtype<-'null'
        gpid2<- 0
      }
    }else{
      mtype<-'null'
      gpid2<- 0
    }
    return(list(mtype=mtype, gpid2=gpid2))
  }#end of function to get managament method

  mtype<- getMtype(gid, dbenv=dbenv)
  while(mtype[[1]]=='MAN' & mtype[[2]]!=0){
  	gid<- mtype[[2]]
    mtype<- getMtype(gid, dbenv=dbenv)
  }
  return(gid)
}
