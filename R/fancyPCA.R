#' fancyPCA function for plotting interactive PCA
#'
#' @importFrom plotly plot_ly
#' @param q is an object that is returned from princomp
#' @param labs are the labels for the points in the plot
#' @param dim is the number of dimensions are the labels for the points
#' @export
#'
fancyPCA<- function(q, labs, dim=3){
  par(mfrow=c(1,2))
  #pca plot
  datdf<- as.data.frame(q$loadings[,c(1:3)])
  if(dim==3){
    p1 <- plotly::plot_ly(data = datdf, x = ~Comp.1, y = ~Comp.2,
             z= ~Comp.3, text=~labs, type='scatter3d',
             mode='markers')
  }else{
    p1 <- plotly::plot_ly(data = datdf, x = ~Comp.1, y = ~Comp.2,
                 text=~labs, type='scatter',
                 mode='markers')
  }
  #scree plot
  datadf2<- data.frame(Percent=(q$sdev^2/tot*100)[1:10],
                       Component=c(1:10))
  p2<- plotly::plot_ly(datadf2, x=~Component,
          y=~Percent, type='scatter', mode='lines')
  return(list(pcplt=p1, scree=p2))
}


