#################
# getMgid
#' Function for getting the Mgid 
#' Mgid is last gid resulting from a derivative method
#' 
#'  Parameters:
#'  @gid A gid
#'  
#'  @return The Mgid 
#'  
################

getMgid<- function(gid){
  
  getMtype<- function(gid){#Function to get the managament method
    if(!is.na(line)){
      grm<- dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",gid))
      tf<- nrow(grm)>0
      if(tf){
        meth<-grm[1,'methn']
        mtype<- dbGetQuery(con, sprintf("SELECT mtype FROM methods WHERE mid=%d",meth))[,1]
        gpid2=grm[1,'gpid2']
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
  
  mtype<- getMtype(gid)
  while(mtype[[1]]=='MAN' & mtype[[2]]!=0){
    line<- mtype[[2]]
    mtype<- getMtype(gid)
  }
  return(line)
}
