#Function to check if someting is in the database
inDatabase<- function(gid){
  if(is.na(gid)){
    tf<- FALSE
  }else{
    rec<- dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",gid))
    tf<- nrow(rec)>0
  }
  return(tf)
}