########################
#The number of equivalent complete generations for several gids
#See https://doi.org/10.1186/1297-9686-45-1
calcEqCg<- function(line){
  calc1EqCg<- function(line, db=NULL){
    cyc<- 0
    p<- setdiff(GetPrnts(line,pastenm=T), 0)
    if(is.null(db)){
      db<- data.frame(line, p, stringsAsFactors = F)
    }
    p<- unlist(strsplit(p, split=","))
    cyc<- cyc+1
    p<- setdiff(p, '0')
    inc<- sum(rep(.5^cyc, length(p)))
    start<- inc
    while(inc>0){
      #cat(start, '\n')
      ixdb<- match(p, db[,1])
      p_db<- na.omit(db[na.omit(ixdb),2])
      if(TRUE %in% is.na(ixdb)){
        srch<- as.numeric(p[is.na(ixdb)])
        p<- as.vector(sapply(srch, GetPrnts, pastenm=T))
        dbn<- data.frame(line=srch, p=p, stringsAsFactors = F)
        db<-rbind(db, dbn)
        p<- append(p, p_db)
      }else{
        p<- p_db
      }
      p<- unlist(strsplit(p, split=","))
      cyc<- cyc+1
      p<- setdiff(p, '0')
      inc<- sum(rep(.5^cyc, length(p)))
      start<- start+inc
    }
    return(list(EqGi=start, db=db, cyc=c(cyc-1)))
  }
  rslts<- c()
  cycs<- c()
  for(i in 1:length(line)){
    cat(i, '\n')
    if(i>1){
      obj<- calc1EqCg(line[i], db=db0)
    }else{
      obj<- calc1EqCg(line[i])
    }
    rslt<- obj$EqGi
    db0<- obj$db
    cyc<- obj$cyc
    cycs<- append(cycs, cyc)
    rslts<- append(rslts, rslt)
  }
  return(list(rslts=rslts, cycs=cycs))  
}
