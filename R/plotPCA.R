#q is a result of princomp
plotPCA<- function(q){
    par(mfrow=c(1,2))
    #pca plot
    tot<- sum(c(q$sdev)^2)
    xlabt<- paste("Comp.1,", format(c(q$sdev[1])^2/tot*100, digits=4), 
                "percent explained", sep=" ")
    ylabt<- paste("Comp.2,", format(c(q$sdev[2])^2/tot*100, digits=4), 
                "percent explained", sep=" ")
    pc<- q$loadings[,c(1,2)]
    plot(pc, main="PCA plot", col='black', ylab=ylabt, xlab=xlabt)
    #scree plot
    plot((q$sdev^2/tot*100)[1:10], type='b',
         ylab='Percent of variation explained', 
         xlab='Component', main='Scree Plot')
    
}



