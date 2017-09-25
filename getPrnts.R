#########
#Get parents of an individaul
getPrnts<- function(line, pastenm=F){
  if(line==0 | is.na(line)){
    prnts<- c(0, 0)
  }else{
    f1<- GetF1(line) #gid of the cros
    if(is.na(f1) | f1==0){
      prnts<- c(0, 0)
    }else{
      f1inf<- dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",f1)) #query of cross
      prnts<- as.numeric(f1inf[1,c(4:5)]) #gids of the parents
    }
  }
  if(pastenm){
    prnts<- paste(prnts, collapse=",")
  }
  return(prnts)
}
