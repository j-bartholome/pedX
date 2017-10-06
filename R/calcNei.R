#' calculate Nei
#'
#' @param a vector of the equivalent complege generations
#' @param a vector of the inbreeding coefficients of the F1s
#' @return a vector of Nei values
#' @export
#'
calcNei<- function(eqcgvec, If1s){
  pwr<- 1/c(eqcgvec -1)
  deltFi<- 1-c(c(1-If1s)^pwr)
  Nei<- 1/c(2*deltFi)
  Nei[which(Nei=='Inf')]<- NA
  return(Nei)
}
