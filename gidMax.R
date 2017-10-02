gidMax<- function(gidvec, dbenv){
  nhits<- c()  
  for(i in 1:length(gidvec)){
    assign('gid', gidvec[i], envir=dbenv)
    nhit<- nrow(with(dbe, dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gpid2=%d",gid))))
    nhits<- append(nhits, nhit)
  }
  return(gidvec[which.max(nhits)])
}