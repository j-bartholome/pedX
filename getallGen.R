#Get pedigree file for a vector of lines
getallGen<- function(lines){
  lines0<- lines
  srh<- lines
  rnd<-0
  while(length(lines)>0){
    rnd<- rnd+1
    Pd<- get1Gen(lines)
    if(!is.null(nrow(Pd))){
      lines<- as.numeric(Pd[which(Pd$gene==0),1]) #founders
    }
    lines<- setdiff(lines, srh)
    srh<- append(srh, lines)
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
