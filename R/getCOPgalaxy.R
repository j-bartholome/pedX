#' getCOPgalaxy wrapper function for getting a sparse relationship (COP) matrix
#'
#' @param pathPed path to pedigree
#' @param pathIds path to ids of interest
#' @export
#' 
getCOPgalaxy<- function(pathPed, pathIds, ordered){
  pd<- read.csv(pathPed, row.names=1)
  ids<- read.csv(pathIds)[,1]
  cp<- getCOP(ids, pd, ordered=ordered)
  save.image(cp, file='COPsparse.RData')
}
