#getCOP function
getCOP<- function(gidvec, ped){
  sp2 <- editPed(ped[,2],ped[,3], ped[,1])
  P<- pedigree(sp2[,2],sp2[,3],sp2[,1])
  aMx <- crossprod(relfactor(P, gidvec))
  return(aMx)
}
