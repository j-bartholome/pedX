#Get pedigrees, (one generation) for a vector of gids
get1Gen<-function(findvec){
  findvec<- na.omit(findvec)
  for(i in 1:length(findvec)){
    if(InDatabase(findvec[i])){
      tb<- tracePedg1(findvec[i])
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
      tb<- c(findvec[i], NA, NA)
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
