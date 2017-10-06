#' get1Gen Function for getting all gids associated with a name
#'
#' @import RPostgreSQL
#' @param nm A vector of names
#' @param dbenv the database connection environment
#' @return a vector of gids
#' @export
#'
getallGids<- function(nm, dbenv){
  nm<- paste('\'',nm, '\'',sep="")
  assign('nm', nm, envir=dbenv)
  gids<- with(dbenv, dbGetQuery(con, sprintf("SELECT * FROM names WHERE nval=%s", nm)))
  if(nrow(gids)>0){
    gids<- gids[,'gid']
  }else{
    gids<- NA
  }
  return(gids)
}
