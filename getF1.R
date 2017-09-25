#########################################
#Function to get gid of the nearest f1 cross
getF1<- function(line){
  if(is.na(line)){
    f1<- NA
  }else{
    if(InDatabase(line)){
      germplasm <- dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",line))
      if(germplasm[,'gnpgs']==-1){
        f1 <- germplasm[1,"gpid1"]
      }
      if(germplasm[,'gnpgs']==2 | germplasm[,'gnpgs']==1){
        f1 <- line
      }
      if(germplasm[,'gnpgs']==0){
        f1<- line
      }
      if(f1==0){
        f1<- NA
      }
    }else{
      f1<- NA 
    }
  }
  
  return(f1)
}
