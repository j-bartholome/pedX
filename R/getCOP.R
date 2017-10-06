#' getCOP Function for getting a sparse relationship (COP) matrix
#'
#' @import pedigreemm
#' @param Ped Sparse pedigree matrix (class pedigree from pedigreemm) OR a data fame
#' @param gidvec a vector of gids of interest
#' @return A sparse relationship matrix
#' @export
#'
getCOP<- function(gidvec, ped){
  if(class(ped)[1]=="data.frame"){
    sp2 <- editPed(ped[,'sire'],ped[,'dam'], ped[,'label'])
    P<- pedigree(sp2[,2],sp2[,3],sp2[,1])
  }
  if(class(ped)[1]=='pedigree'){
    P<- ped
  }
  rect <- Diagonal(x = sqrt(Dmat(P))) %*% solve(t(as(P,
    "sparseMatrix")), as(factor(P@label, levels = P@label), "sparseMatrix"))
  tmpA <- crossprod(rect)
  tmp <- match(gidvec, P@label)
  tmpA <- tmpA[tmp, tmp]
  return(tmpA)
}
