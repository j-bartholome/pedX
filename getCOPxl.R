#' getCOPxl function for getting a large sparce relationship (COP) matrix
#'
#'  Parameters:
#'  @param P Sparse pedigree matrix (class pedigree from pedigreemm)
#'  @param Cfun is the environment containing the C++ code for the matrix multiplication
#'  @return A sparse relationship matrix
#'  @export
#'
getCOPxl<- function(P, Cfun){
  assign('rect', Diagonal(x = sqrt(Dmat(P))) %*%
           solve(t(as(P, "sparseMatrix")),
            as(factor(P@label, levels = P@label),
            "sparseMatrix")),envir=Cfun)
  Amat <- with(Cfun, sparse1(rect)[[1]])
  #dimnames(Amat)[[1]] <- dimnames(Amat)[[2]] <- P@label
  return(Amat)
}


