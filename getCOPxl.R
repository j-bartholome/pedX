#################
# getCOPxl
#' Function for getting a large sparce relationship (COP) matrix
#' from a sparse pedigree matrix. This uses some matrix multiplication
#' in C++ using Eigen, its still not faster than R
#' 
#'  Parameters:
#'  @P Sparse pedigree matrix (class pedigree from pedigreemm)
#'  @ Cfun is the environment containing the 
#'  C++ code for the matrix multiplication step, this is created using 
#'  defCfcns
#'  
#'  @return A sparse relationship matrix
#'  
################
getCOPxl<- function(P, Cfun){
  assign('rect', Diagonal(x = sqrt(Dmat(P))) %*% 
           solve(t(as(P, "sparseMatrix")), 
            as(factor(P@label, levels = P@label), 
            "sparseMatrix")),envir=Cfun)
  Amat <- with(Cfun, sparse1(rect)[[1]])
  #dimnames(Amat)[[1]] <- dimnames(Amat)[[2]] <- P@label
  return(Amat)
}


