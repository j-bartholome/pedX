#' F1inbred Function to identify the inbreedign coefficent of the F1 progenitor
#'
#' @param dbenv the database connection environment
#' @param gidvec A vector of numeric gids
#' @param cp a coefficent of parentage matrix
#' @return a vector of inbreeding coefficents
#' @export
#'
F1inbred<- function(cp, gidvec, dbenv){
  cross<- sapply(gidvec, getF1, dbenv=db)
  ix<- match(cross, row.names(cp))
  I<- diag(cp[ix, ix])-1
  return(I)
}



