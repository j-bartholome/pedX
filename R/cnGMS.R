#' Setting the connection
#'
#' @import RPostgreSQL
#'
#' @param usr a character string for the "username"
#' @param pws a character string for the "password"
#' @param host a character string for the ip address of the host
#' @return the function environment
#' @export
#'
cnGMS<- function(usr, psw, host){
  yourusername <- usr
  yourpassword <- psw

  #Driver is PostgreSQL not RPostgreSQL
  drv <- dbDriver("PostgreSQL")

  #Replace user and pass. Leave dbname, hostname, and port as is.
  con <- dbConnect(
    drv,
    dbname = "central_iris",
    host = host,
    port = 5432,
    user = yourusername,
    password = yourpassword
  )
  df_postgres<- dbGetQuery(con, "SET search_path = gms")
return(environment())
}
