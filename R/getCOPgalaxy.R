#' getCOPgalaxy wrapper function for getting a sparse relationship (COP) matrix
#'
#' @import RGalaxy
#' @param pathPed character path to pedigree
#' @param pathIds character path to ids of interest
#' @param ordered boolean if pedigree is ordered or not
#' @export
#' 
#' (number1=GalaxyNumericParam(required=TRUE),
getCOPgalaxy<- function(pathPed=GalaxyCharacterParam(required=TRUE), 
                        pathIds= GalaxyCharacterParam(required=TRUE), 
                        ordered= GalaxyLogicalParam(required=TRUE),
                        outputfile= GalaxyOutput('COPsparse', 'RData')){
  pd<- read.csv(pathPed, row.names=1)
  ids<- read.csv(pathIds)[,1]
  cp<- getCOP(ids, pd, ordered=ordered)
  save.image(cp, file=outputfile)
}
