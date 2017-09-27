#getCOP function
getCOP<- function(gidvec, ped){
  sp2 <- editPed(ped[,2],ped[,3], ped[,1])
  P<- pedigree(sp2[,2],sp2[,3],sp2[,1])
  tmpA<- getA(P)
  tmp <- match(gidvec, P@label)
  tmpA <- tmpA[tmp, tmp]
  return(tmpA)
}