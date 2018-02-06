#' getCOPgalaxy wrapper function for getting a relationship matrix
#'
#' @import nadiv
#' @import RGalaxy
#' @param pathPed galaxy input file for pedigree
#' @param pathIds galaxy input file for ids of interest
#' @param ordered boolean if pedigree is ordered or not
#' @export
#'
getCOPgalaxy<- function(pathPed=GalaxyInputFile(required=TRUE, formatFilter=character(0)),
                        pathIds= GalaxyInputFile(required=TRUE, formatFilter=character(0)),
                        ordered= GalaxyLogicalParam(required=TRUE),
                        outputfile= GalaxyOutput('relmat', 'RData')){
  pd<- read.csv(pathPed, row.names=1)
  ids<- read.csv(pathIds)[,1]
  	if(!ordered){
  		pd<- prepPed(pd)
	}
	pdsub<- prunePed(pd, ids)[,c(1:3)]
	ix<- match(ids, pdsub[,1])
	A<- as.matrix(makeA(pdsub))[ix, ix]
  	save(A, file=outputfile)
}
