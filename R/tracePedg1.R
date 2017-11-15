#' tracePedg1 function for tracing one generation of a pedigree
#'
#' @import RPostgreSQL
#' @import pedigreemm
#' @param gid A gid
#' @param dbenv the database connection environment
#' @return A dataframe of query results containg complete generation traced
#' @export
#'
tracePedg1<- function(gid, dbenv){
  tab<- NA
  assign('gid', gid, envir=dbenv)
  germplasm <- with(dbenv,dbGetQuery(con, sprintf("SELECT * FROM germplsm WHERE gid=%d",gid)))
  if(nrow(germplasm)>0){
    nprgntr<- germplasm[,'gnpgs'] #number of progeneters
    tab<- germplasm
    if(nprgntr!=0){ #if number of progenetors is not zero
      if(nprgntr>2){ #if more than to progenetors
        germplasm[1,"gpid2"]<-0 #make male parent missing
      }
      if(nprgntr==-1){ #if derivitave methods
        f1 <- germplasm[1,"gpid1"] #get the id for the f1 cross
        assign('f1', f1, envir=dbenv)
        f1cross <- with(dbenv,dbGetQuery(con,  #look up f1 cross
                              sprintf("SELECT * FROM germplsm WHERE gid=%d",f1)))
      }
      tab<- germplasm #initialize results table
      while(nrow(germplasm)>0){ #if germplasm lookup contains records
        if(germplasm[,'gnpgs']==-1){ #if germplasm lookup is deriv.
          assign('gpid2', germplasm[,'gpid2'], envir=dbenv)
          germplasm <- with(dbenv,dbGetQuery(con, #look up the pedigree of the parent
                                  sprintf("SELECT * FROM germplsm WHERE gid=%d",gpid2)))
          tab<- rbind(tab, germplasm) #add the immediate parent ped to the table
        }else{ #if not deriv. stop
          break
        }
      }
      if(nprgntr==-1){#if derivative
        tab<- as.matrix(rbind(tab, f1cross)) #add f1 cross to the table
      }else{ #make the cross
        tab<-germplasm
      }
    }#if numb of progenetors is missing
  }#if record doesnt exist
  return(tab)
}
