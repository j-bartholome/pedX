## Synopsis

This package enables you to get useful information from pedigrees stored in gms. 
It enables calculation of:

* coefficients of kinship (aka COP)
* inbreeding coefficients of lines and populations,
* numbers of crossing cycles
* numbers of 'equivalent complete generations' (EqCG)
* effective population size (Ne) 

It also enables the extraction of source gids (aka MGIDs) which are useful 
for combining data across multiple experiments for analysis in legacy datasets. 
The packages RPostgreSQL and pedigreemm are required.

## Motivation

I started this to solve my personal analysis needs in support of the irrigated breeding program, especially in order to estimate coefficients of kinship more quickly than the ICIS application. 

## Acknowledgement

Thanks to Miguel Abriol for helping with the postgresql queries
