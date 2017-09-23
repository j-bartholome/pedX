#################
# get1Gen
#' Function for getting one generation of a pedigree
#' 
#'  Parameters:
#'  @gidvec A vector of gids
#'  
#'  @return A dataframe with the pedigree
#'  
################
get1Gen<-function(gidvec){
  gidvec<- na.omit(gidvec)
  for(i in 1:length(gidvec)){
    if(inDatabase(gidvec[i])){
      tb<- tracePedg1(gidvec[i])
      ixsA<- which(tb[,3]==-1 & tb[,5]!=0)
      ixsB<- which(tb[,3]==-1 & tb[,5]==0)
      if(length(ixsA)>0){
        tb[ixsA, 4]<- tb[ixsA, 5]
      }
      if(length(ixsB)>0){
        tb[ixsB, 5]<- tb[ixsB, 4]
      }
      tb<- tb[,c('gid','gpid1','gpid2')]
    }else{
      tb<- c(gidvec[i], NA, NA)
      names(tb)<-c('gid','gpid1','gpid2')
    }
    if(i==1){
      tbs<- tb
    }else{
      tbs<- rbind(tbs, tb) 
    }
  }
  if(!is.null(dim(tbs))){
    tbs[which(tbs[,'gpid1']==0),'gpid1']<- NA
    tbs[which(tbs[,'gpid2']==0),'gpid2']<- NA
    tbs2<- unique(tbs[,c('gid','gpid1','gpid2')])
    PD<- editPed(tbs2[,'gpid1'], tbs2[,'gpid2'], tbs2[,'gid'])
  }else{
    tbs[which(tbs==0)]<- NA
    tbs<- c(tbs, 0)
    names(tbs)<- c('gid','gpid1','gpid2', 'gene')
    PD<- tbs
  }
  return(PD)
}
