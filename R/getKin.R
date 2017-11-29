#' getKin Function for getting a kinship matrix and visualization
#'
#' @import nadiv
#' @import Matrix
#' @import ggplot2
#' @import reshape
#' @param vecint gid vector
#' @param desigs designation vector
#' @param file pedigree file name
#' @param sorted if the pedgree is sorted or not
#' @param viewer if visualization is required
#' @param outcsv if csv output is required
#' @param colL low color
#' @param colH high color
#' @export
#'
getKin<- function(vecint, desigs, file, sorted=TRUE, viewer=TRUE, outcsv=TRUE, colL="darkseagreen3", colH="cornsilk"){
	P<- read.csv(file, row.names=1)
	if(!sorted){
  		P<- prepPed(P)
  		write.csv(P, file=file)
	}
	pdsub<- suppressWarnings(prunePed(P, vecint)[,c(1:3)])
	ix<- match(vecint, pdsub[,1])
	A<- as.matrix(makeA(pdsub))[ix, ix]/2
	colnames(A)<- desigs
	row.names(A)<- desigs
	if(outcsv){
		write.csv(A, file='kinship.csv')
	}
	if(viewer){
		tab<-melt(A)
		tab[,1]<- as.character(tab[,1])
		tab[,2]<- as.character(tab[,2])
		tab[,3]<- tab[,3]
		colnames(tab)<-c('Individual 1', "Individual 2", 'Kinship')
		p <- ggplot(tab, aes(`Individual 1`, `Individual 2`)) + geom_tile(aes(fill = Kinship),
			colour = "white") + scale_fill_gradient(low = colL, high = colH)+
			geom_text(size=5,aes(label=round(Kinship,2))) + theme(panel.background = element_blank())+
			theme(axis.text.x = element_text(color="black", size=14, angle=45, hjust=1),
      		axis.text.y = element_text(color="black", size=14, angle=45, hjust=1))+labs(x="", y="")
		p
	}
}

