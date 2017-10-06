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