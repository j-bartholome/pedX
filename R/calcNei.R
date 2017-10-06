#calculate Nei
calcNei<- function(eqcgvec, If1s){
  pwr<- 1/c(eqcgvec -1)
  deltFi<- 1-c(c(1-If1s)^pwr)
  Nei<- 1/c(2*deltFi)
  Nei[which(Nei=='Inf')]<- NA
  return(Nei)
}
