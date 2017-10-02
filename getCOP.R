#################
# getCOP
#' Function for getting a sparce relationship (COP) matrix
#' 
#'  Parameters:
#'  @P Sparse pedigree matrix (class pedigree from pedigreemm) 
#'  OR a data fame
#'  @gidvec a vector of gids of interest
#'  
#'  @return A sparse relationship matrix
#'  
#'  This is actually quite fast
################
getCOP<- function(gidvec, ped){
  if(class(ped)[1]=="data.frame"){
    sp2 <- editPed(ped[,2],ped[,3], ped[,1])
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