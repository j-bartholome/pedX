#################
# kinNe
#' Function for calculating Ne using a kinship matrix
#' 
#'  Parameters:
#'  @param A kinship matrix
#'  @param selIDs vector containing selected gids
#'  
################

kinNe<- function(A, selIDs){
  ixA<- na.omit(match(as.character(selIDs), row.names(A)))
  Asb<- as.matrix(A[ixA, ixA])
  #First gen kin average
  F0<- mean(Asb[upper.tri(Asb)])
  
  #Get second gen kin average
  dosediag<- sum(1:c(ncol(Asb)-2)) ##Dosage of the diagonal
  doseoffdiag<- c(ncol(Asb)-2)*ncol(Asb) #Dosage of the off diagonala 
  z<- Asb[upper.tri(Asb)]*doseoffdiag
  x<- diag(Asb)*dosediag
  n<- length(z)*doseoffdiag + length(x)*dosediag
  F1<- sum(c(z, x))/n

  #compute change in F
  deltaF <- (F1 - F0)/(1 - F0)
  
  #compute Ne
  Ne <- 1/(2*deltaF)
  
  cat("F0: ", F0, "\n")
  cat("F1: ", F1, "\n")
  cat("Ne: ", Ne, "\n")
  
  return(list(F0 = F0, F1 = F1, Ne = Ne))  
}
