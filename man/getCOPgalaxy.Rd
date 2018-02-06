\name{getCOPgalaxy}
\alias{getCOPgalaxy}
\title{getCOPgalaxy wrapper function for getting a sparse relationship (COP) matrix}
\usage{
getCOPgalaxy<- function(pathPed=GalaxyCharacterParam(required=TRUE), 
                        pathIds= GalaxyCharacterParam(required=TRUE), 
                        ordered= GalaxyLogicalParam(required=TRUE),
                        outputfile= GalaxyOutput('COPsparse', 'RData'))
}
\arguments{
\item{pathPed}{character path to pedigree}

\item{pathIds}{character path to ids of interest}
}

\description{
getCOPgalaxy Function for getting a sparse relationship (COP) matrix
}
