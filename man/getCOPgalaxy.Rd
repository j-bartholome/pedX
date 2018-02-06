\name{getCOPgalaxy}
\alias{getCOPgalaxy}
\title{Get pedigree relationship matrix}
\usage{
getCOPgalaxy<- function(pathPed=GalaxyInputFile(required=TRUE, formatFilter=character(0)),
                        pathIds= GalaxyInputFile(required=TRUE, formatFilter=character(0)),
                        ordered= GalaxyLogicalParam(required=TRUE),
                        outputfile= GalaxyOutput('relmat', 'RData'))
}
\arguments{
\item{pathPed}{galaxy input file for pedigree}

\item{pathIds}{galaxy input file for ids of interest}
}

\description{
Function for getting a relationship matrix from pedigree
}
