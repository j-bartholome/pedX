#' getCOPgalaxy wrapper function for getting a sparse relationship (COP) matrix
#'
#' @import RGalaxy
#' @param pathPed galaxy input file for pedigree
#' @param pathIds galaxy input file for ids of interest
#' @param ordered boolean if pedigree is ordered or not
#' @export
#'
getCOPgalaxy<- function(pathPed=GalaxyInputFile(required=TRUE, formatFilter=character(0)),
                        pathIds= GalaxyInputFile(required=TRUE, formatFilter=character(0)),
                        ordered= GalaxyLogicalParam(required=TRUE),
                        outputfile= GalaxyOutput('COPsparse', 'RData')){
  pd<- read.csv(pathPed, row.names=1)
  ids<- read.csv(pathIds)[,1]
  cp<- getCOP(ids, pd, ordered=ordered)
  save.image(cp, file=outputfile)
}
