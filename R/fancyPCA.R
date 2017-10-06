#q is a result of princomp
#labs are the labels for the points
#dim is the number of dimensions are the labels for the points
fancyPCA<- function(q, labs, dim=3){
  par(mfrow=c(1,2))
  #pca plot
  datdf<- as.data.frame(q$loadings[,c(1:3)])
  if(dim==3){
    p1 <- plot_ly(data = datdf, x = ~Comp.1, y = ~Comp.2, 
             z= ~Comp.3, text=~labs, type='scatter3d',
             mode='markers') 
  }else{
    p1 <- plot_ly(data = datdf, x = ~Comp.1, y = ~Comp.2, 
                 text=~labs, type='scatter',
                 mode='markers') 
  }
  #scree plot
  datadf2<- data.frame(Percent=(q$sdev^2/tot*100)[1:10], 
                       Component=c(1:10))
  p2<- plot_ly(datadf2, x=~Component, 
          y=~Percent, type='scatter', mode='lines')
  return(list(pcplt=p1, scree=p2))
}


