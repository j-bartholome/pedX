#' Setting the connection
#'
#' @import RPostgreSQL
#' @param usr character username
#' @param pws character passward
#' @return the object which is the function environment
#' @export
#'
cnGMS<- function(usr, psw){
  yourusername= usr
  yourpassword= psw

  #Driver is PostgreSQL not RPostgreSQL
  drv <- dbDriver("PostgreSQL")

  #Replace user and pass. Leave dbname, hostname, and port as is.
  con <- dbConnect(drv, dbname = "central_iris",
                 host = "172.29.4.52", port = 5432,
                 user = yourusername, password = yourpassword)
  df_postgres<- dbGetQuery(con, "SET search_path = gms")
return(environment())
}
