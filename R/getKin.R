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
#' @return list of plot and the matrix
#' @export
#'
getKin<- function(vecint, desigs, file=dt, sorted=TRUE, viewer=TRUE){
	if(is.character(file)){
    dt<- read.csv(file, row.names=1)
	}else{
	  dt<- file
	}
	if(!sorted){
  		dt<- prepPed(dt)
  		write.csv(dt, file=file)
	}
	pdsub<- prunePed(dt, vecint)[,c(1:3)]
	ix<- match(vecint, pdsub[,1])
	A<- as.matrix(makeA(pdsub))[ix, ix]/2
	colnames(A)<- desigs
	row.names(A)<- desigs
	Aix<- heatorder(A)
	A<- A[Aix, Aix]
	if(viewer){
		tab<-melt(A)
		tab[,1]<- as.character(tab[,1])
		tab[,2]<- as.character(tab[,2])
		tab[,3]<- tab[,3]
		colnames(tab)<-c('Individual 1', "Individual 2", 'Kinship')      		
      p <- ggplot(tab, aes(`Individual 1`, `Individual 2`)) + geom_tile(aes(fill = Kinship))+
      	geom_text(size=5, aes(label=round(Kinship,2))) + 
      	scale_fill_gradientn(colours = heat_hcl(12, c=c(80,30), l=c(30,90), power=c(1/5, 1.5))[c(12:1)], 
      	values  = rescale(c(min(tab$Kinship), max(tab$Kinship)))) + xlab(NULL) + ylab(NULL) + 
      	theme(axis.text.x = element_text(size=14, angle = 45, hjust = 1),
      	axis.text.y = element_text(size=14, angle = 45, hjust = 1))+ theme(panel.background = element_blank()+
      	theme(legend.position="none"))
    
	}
	
return(list(p=p, A=A, tab=tab))
}

