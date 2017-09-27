#calc inbreeding coefficient of the F1 progenitor
F1inbred<- function(cp, gidvec, dbenv){
  cross<- sapply(gidvec, getF1, dbenv=db)
  ix<- match(cross, row.names(cp))
  I<- diag(cp[ix, ix])-1
  return(I)
}



