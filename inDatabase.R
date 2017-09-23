#################
# inDatabase
#' Function to check if something is in the database
#' 
#'  Parameters:
#'  @gid A gid
#'  
#'  @return TRUE or FALSE 
#'  
################

inDatabase<- function(gid){
  if(is.na(gid)){
    tf<- FALSE
  }else{
    rec<- dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",gid))
    tf<- nrow(rec)>0
  }
  return(tf)
}