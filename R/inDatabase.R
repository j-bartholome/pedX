#################
# inDatabase
#' Function to check if something is in the database
#' 
#'  Parameters:
#'  @gid A gid
#'  @dbenv environment with connection to datbase
#'  
#'  @return TRUE or FALSE 
#'  
################

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