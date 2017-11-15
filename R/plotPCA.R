#' plotPCA function for making a two-paneled pca plot
#'
#' @param q is a result of princomp
#' @export
#'
plotPCA<- function(q, subgroup, colr='red'){
    par(mfrow=c(1,2))
    #pca plot
    tot<- sum(c(q$sdev)^2)
    xlabt<- paste("Comp.1,", format(c(q$sdev[1])^2/tot*100, digits=4),
                "percent explained", sep=" ")
    ylabt<- paste("Comp.2,", format(c(q$sdev[2])^2/tot*100, digits=4),
                "percent explained", sep=" ")
    pc<- q$loadings[,c(1,2)]
    plot(pc, main="PCA plot", col='black', ylab=ylabt, xlab=xlabt)
    points(pc[subgroup,1], pc[subgroup,2],col=colr)
    #scree plot
    plot((q$sdev^2/tot*100)[1:10], type='b',
         ylab='Percent of variation explained',
         xlab='Component', main='Scree Plot')

}



