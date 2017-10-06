#' Puts the C++ code in the environment for use later
#'
#' @importFrom imports cxxfunction
#' @return the environment with the C++ function
#' @export
#'

defCfcns<-function(){
  incl<- 'using Eigen::LLT;
  using Eigen::Lower;
  using Eigen::Map;
  using Eigen::MatrixXd;
  using Eigen::MatrixXi;
  using Eigen::Upper;
  using Eigen::VectorXd;
  typedef Map<MatrixXd> MapMatd;
  typedef Map<MatrixXi> MapMati;
  typedef Map<VectorXd> MapVecd;
  '
  sparseProdCpp<- 'using Eigen::MappedSparseMatrix;
  using Eigen::SparseMatrix;
  const MappedSparseMatrix<double> A(as<MappedSparseMatrix<double> >(AA));
  const SparseMatrix<double> Acr(A.adjoint() * A);
  return List::create(Named("Acr") = Acr.triangularView<Lower>());'
  sparse1 <- cxxfunction(signature(AA = "dgCMatrix"),
                         sparseProdCpp, "RcppEigen", incl)
  return(environment())
}

