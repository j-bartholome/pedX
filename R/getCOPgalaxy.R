#' getCOPgalaxy wrapper function for getting a sparse relationship (COP) matrix
#'
#' @import RGalaxy
#' @param pathPed character path to pedigree
#' @param pathIds character path to ids of interest
#' @param ordered boolean if pedigree is ordered or not
#' @export
#' 
getCOPgalaxy<- function(pathPed, pathIds, ordered){
  pd<- read.csv(GalaxyCharacterParam(pathPed), row.names=1)
  ids<- read.csv(GalaxyCharacterParam((pathIds))[,1]
  cp<- getCOP(ids, pd, ordered=ordered)
  save.image(cp, file='COPsparse.RData')
}
