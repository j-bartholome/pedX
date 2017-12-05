#' heatorder Function to order a kinship matrix
#'
#' @param x a kinship matrix
#' @return vector of indices to re-order the kinship matrix
#' @export
#'
heatorder<- function (x){
  di <- dim(x)
  nr <- di[1L]
  nc <- di[2L]
  Rowv <- rowMeans(x, na.rm = TRUE)
  Colv <- colMeans(x, na.rm = TRUE)
  hcc <- hclust(dist(x))
  ddc <- as.dendrogram(hcc)
  ddc <- reorder(ddc, Colv)
  colInd <- order.dendrogram(ddc)
  return(ix=colInd)
}