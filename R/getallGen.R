#' getallGen function for getting all generations of a pedigree
#'
#' @param gidvec A vector of gids
#' @param dbenv the database connection environment
#' @return A dataframe with the pedigree
#' @export
#'
getallGen<- function(gidvec, dbenv){
  lines0<- gidvec
  srh<- gidvec
  rnd<-0
  while(length(gidvec)>0){
    rnd<- rnd+1
    Pd<- get1Gen(gidvec, dbenv=dbenv)
    if(!is.null(nrow(Pd))){
      gidvec<- as.numeric(Pd[which(Pd$gene==0),1]) #founders
    }
    gidvec<- setdiff(gidvec, srh)
    srh<- append(srh, gidvec)
    if(rnd>1){
      Pds<- rbind(Pds, Pd)
    }else{
      Pds<- Pd
    }
  }
  if(!is.null(nrow(Pds))){
    Pds<- Pds[-setdiff(which(is.na(Pds[,1]) | is.na(Pds[,2])),match(lines0, Pds[,1])),]
    Pds<- unique(Pds[,c(1:3)])
    Pds[which(Pds[,2]==0),2]<- NA
    Pds[which(Pds[,3]==0),3]<- NA
    Pds2<-editPed(Pds[,'sire'], Pds[,'dam'], Pds[,'label'])
  }else{
    Pds2<- Pds
  }

  return(Pds2)
}
