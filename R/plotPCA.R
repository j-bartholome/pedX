#' plotPCA function for making a two-paneled pca plot
#'
#' @param q is a result of princomp
#' @export
#'
plotPCA<- function(q, subgroup1, colr1='red', subgroup2, colr2='blue'){
    par(mfrow=c(1,2))
    #pca plot
    tot<- sum(c(q$sdev)^2)
    xlabt<- paste("Comp.1,", format(c(q$sdev[1])^2/tot*100, digits=4),
                "percent explained", sep=" ")
    ylabt<- paste("Comp.2,", format(c(q$sdev[2])^2/tot*100, digits=4),
                "percent explained", sep=" ")
    pc<- q$loadings[,c(1,2)]
    plot(pc, main="PCA plot", col='black', ylab=ylabt, xlab=xlabt)
    points(pc[subgroup1,1], pc[subgroup1,2],col=colr1)
    points(pc[subgroup2,1], pc[subgroup2,2],col=colr2)
    #scree plot
    plot((q$sdev^2/tot*100)[1:10], type='b',
         ylab='Percent of variation explained',
         xlab='Component', main='Scree Plot')

}



