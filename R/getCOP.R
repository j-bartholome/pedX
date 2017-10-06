#' getCOP Function for getting a sparse relationship (COP) matrix
#'
#' @import pedigreemm
#' @import Matrix
#' @param Ped Sparse pedigree matrix (class pedigree from pedigreemm) OR a data fame
#' @param gidvec a vector of gids of interest
#' @return A sparse relationship matrix
#' @export
#'
getCOP<- function(gidvec=NULL, ped){
  if(is.null(gidvec)){
    gidvec<- ped[,'label']
  }
  if(class(ped)[1]=="data.frame"){
    sp2 <- editPed(ped[,'sire'],ped[,'dam'], ped[,'label'])
    P<- pedigree(sp2[,2],sp2[,3],sp2[,1])
  }
  if(class(ped)[1]=='pedigree'){
    P<- ped
  }
  rect <- Diagonal(x = sqrt(Dmat(P))) %*% Matrix::solve(Matrix::t(as(P,"sparseMatrix")), as(factor(P@label, levels = P@label), "sparseMatrix"))
  tmpA <- Matrix::crossprod(rect)
  tmp <- match(gidvec, P@label)
  tmpA <- tmpA[tmp, tmp]
  return(tmpA)
}
