#' Sample selection function for selecting representative samples of a target population
#'
#' @param cp a relationship matrx
#' @param cand is a vector of indices pointing to the candidates for the sample
#' @param target is a vector of indices pointing to the target population
#' @param nchoose is the desired sample size
#' @param herit is the heritability of the trait
#' @param ntry is the number of iterations of the exchange algorithm
#' @param nrep is the number of repitions of the exchange algorithm
#'
sampleSelect<- function(cp, cand, target, nchoose, herit=0.5, ntry=300, nrep=30){
  CDmean=function(A, Tindex0, Pindex0, h2, Contrast='Pop'){
    lambda=(1-h2)/h2
    contrasteNonPheno=function(NotSampled,Nind,Nind_in_Sample){
      mat=matrix(-1/Nind,Nind,Nind-Nind_in_Sample)
      for (i in 1:ncol(mat)) {
        mat[NotSampled[i],i]=1-1/Nind
      }
      return(mat)
    }
    A<- as.matrix(A)
    matA1<- A[c(Tindex0, Pindex0), c(Tindex0, Pindex0)]
    Tindex<- c(1:length(Tindex0))
    Pindex<- (length(Tindex0)+1):nrow(matA1)
    invA1= solve(matA1)
    X<- as.matrix(matrix(1, nrow=nrow(matA1))[Tindex,])
    Z<- matrix(0, nrow(X), nrow(matA1))
    for(i in 1:length(Tindex)){
      Z[i, i]=1
    }
    Z<- as.matrix(Z)
    Ident<-diag(nrow(X))
    M<-Ident- (X%*%solve(t(X)%*%X) %*% t(X) )

    if(Contrast=="Pop"){
      T= contrasteNonPheno(Pindex, nrow(matA1), length(Tindex))
      T[Tindex,]= matrix(0, nrow=length(Tindex), ncol=ncol(T))
    }else{
      T= diag(nrow(matA1))
    }

    matCD<-(t(T)%*%(matA1-lambda*solve(t(Z)%*%M%*%Z + lambda*invA1))%*%T)/(t(T)%*%matA1%*%T)
    if(Contrast=="Pop"){
      CD=diag(matCD)
    }else{
      CD= diag(matCD)[-Tindex]
    }
    rvec=mean(CD)
    return(rvec)
  }

  rslt<- list()
  cds<- c()
  numb<- 0
  for(i in 1:nrep){
    tsamp<- sample(cand, 50)
    q<- CDmean(cp, Tindex0=tsamp, Pindex0=target, h2=herit, Contrast='Pop')
    numb<- numb+1
    # Exchange algorithm
    cpt2<- 1
    cpt<- 0
    #save rslts
    rslt[[numb]]<-tsamp
    cds<- append(cds, q)
    ##Start of the loop
    while (cpt2<ntry) {  # Make sure that Ntry is enough in your case
      cpt2<- cpt2+1
      q0<- q
      tsamp0<- tsamp
      # Remove one individual (randomly choosen) from the TP:
      minus<- sample(tsamp)[-1]
      plus<- sample(setdiff(cand, tsamp))[1]
      tsamp<- c(minus, plus)
      q<- CDmean(cp, Tindex0=tsamp, Pindex0=target, h2=herit, Contrast='Pop')
      numb<- numb+1
      rslt[[numb]]<-tsamp
      if(q<q0){
        tsamp<- tsamp0
      }
      cds<- append(cds, q)
      pctcomp<- numb/(ntry*nrep) *100
      if((pctcomp %% 10)==0){
        cat(paste(pctcomp, "% complete", sep=""), "\n")
      }
    }
  }
  out<- list(bestsample=rslt[[which.max(cds)]], allsamps=rslt, allcdmeans=cds)
  return(out)
}
