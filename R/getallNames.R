#' get1Gen Function for getting all gids associated with a name
#'
#' @import RPostgreSQL
#' @param gid A vector of gids
#' @param dbenv the database connection environment
#' @param type a numeric name type
#' @return a vector of names
#' @export
#'
#Get all names associated with a gid
getallNames<- function(gid, dbenv, type=6){
  assign('gid', gid, envir=dbenv)
  assign('type', type, envir=dbenv)
  if(!is.na(gid)){
    if(type=='all'){
      nms<- with(dbenv, dbGetQuery(con,
                    sprintf("SELECT nval FROM names WHERE nstat!=9 AND gid=%s", gid)))
    }else{
      nms<- with(dbenv, dbGetQuery(con,
                sprintf("SELECT nval FROM names WHERE nstat!=9 AND gid=%s AND ntype=%s", gid, type)))
    }

    if(nrow(nms)>0){
      nms<- nms[,1]
    }else{
      nms<- gid
    }
  }else{
    nms<- NA
  }
  return(nms)
}
