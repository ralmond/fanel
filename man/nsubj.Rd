\name{nsubj}
\alias{nsubj}
\alias{nsubj<-}
\alias{isubj}
\alias{isubj<-}
\alias{nocc}
\alias{nocc<-}
\alias{maxocc}
\alias{maxocc<-}
\alias{minocc}
\alias{minocc<-}
\alias{nmodels}
\alias{nquad}
\alias{nquad<-}
\alias{dname}
\alias{dname<-}
\alias{tname}
\alias{tname<-}
\alias{wname}
\alias{wname<-}
\alias{getTime}
\alias{getTime<-}
\alias{getDT}
\alias{getDT<-}



\title{Accesses shape information about fanel objects}
\description{

  Accesses the number of subject or occassions for a fanel data object.
  For objects where the true number of subjects or number of occasions, the
  logical number can be set.

}
\usage{
nsubj(obj)
nsubj(obj) <- value
isubj(obj)
isubj(obj) <- value
nocc(obj)
nocc(obj) <- value
maxocc(obj)
maxocc(obj) <- value
minocc(obj)
minocc(obj) <- value
nmodels(obj)
nquad(obj)
nquad(obj) <- value
dname(obj)
dname(obj) <- value
tname(obj)
tname(obj) <- value
wname(obj)
wname(obj) <- value
getTime(obj)
getTime(obj) <- value
getDT(obj)
getDT(obj) <- value

}
\arguments{
  \item{obj}{A \code{\linkS4class{Panmat}}, \code{\linkS4class{Panel_Frame}} or
    \code{\link{Panel_Data}} object.}
  \item{value}{An integer giving the new value for the statistic.}
}
\value{
  An integer giving the number of subjects, index of focus subject,
  number of occasions, or the minimum/maximum occasion index.
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{Panmat}}, \code{\linkS4class{Panel_Frame}}
    \code{\link{Panel_Data}}
}
\examples{
}
\keyword{ attribute }
\concept{ panel_data }

