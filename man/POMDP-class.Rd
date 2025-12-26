\name{POMDP-class}
\docType{class}
\alias{POMDP-class}
\alias{POMDP}
\alias{population}
\alias{population<-}
\alias{activities}
\alias{activities<-}
\alias{evidence}
\alias{evidence<-}
\alias{as_longform,POMDP-method}
\alias{mstep,POMDP-method}
\alias{drawData.POMDP}
\alias{drawInitial.POMDP}
\alias{drawGrowth.POMDP}
\alias{evalEvidence.POMDP}
\alias{advanceWeights.POMDP}
\alias{retreatWeights.POMDP}
\alias{probInit.POMDP}
\alias{nsubj.POMDP}
\alias{nocc.POMDP}
\alias{minocc.POMDP}
\alias{maxocc.POMDP}
\alias{nmodels.POMDP}


\title{Class \code{"POMDP"}}
\description{

  This is a container for a complete model description.  It has three
  components:
  \describe{
    \item{\code{population()}}{A \code{\linkS4class{Population}}
      contianing \code{\link{PopulationModel}}s.}
    \item{\code{activities()}}{An \code{\linkS4class{Activities}}
      contianing \code{\link{GrowthModel}}s.}
    \item{\code{evidence()}}{An \code{\linkS4class{Evidence}}
      contianing \code{\link{EvidenceModel}}s.}
  }

}
\section{Fields}{
  \describe{
    \item{\code{name}:}{A character vector used as an identifier.}
    \item{\code{components}:}{A named list with three components,
      \code{population}, \code{activities}, \code{evidence}.}
  }

  The three components can be accessed through the \code{population()}
  (\code{population<-()}), \code{activities()} (\code{activities()})
  and \code{evidence()} (\code{evidence<-()}) accessor functions.

}
\section{Methods}{
  \describe{
    \item{\code{nsubj}}{\code{(obj)}:  Returns the number of subjects
      (see \code{\link{nsubj}}.}
    \item{\code{nmodels}}{\code{(obj)}:  Returns the number of models.}
    \item{\code{nocc}}{\code{(obj)}:  Returns the number of occasions.}
    \item{\code{minocc}}{\code{(obj)}:  Returns the minimum occasion number.}
    \item{\code{maxocc}}{\code{(obj)}:  Returns the maximum occasion number.}
    \item{\code{drawInitial}}{\code{(model, isubj, npart, covar=NULL)}:
      Draws initial values for latent variables. (see
      \code{\link{drawData}})} 
    \item{\code{probInit}}{\code{(model, isubj, thetas, covar=NULL)}:
      Calculates probabilities of initial values.}
    \item{\code{drawGrowth}}{\code{(model, isubj, iocc, theta,
	covar=NULL)}: Draw next value for latent variable.} 
    \item{\code{advanceWeights}}{\code{(model, isubj, iocc, lweights,
	covar=NULL)}: Update weights going forwards.} 
    \item{\code{retreatWeights}}{\code{(model, isubj, iocc, rweights,
	covar=NULL)}: Update weights going backwards.} 
    \item{\code{drawData}}{\code{(model, isubj, iocc, theta,
	covar=NULL)}: Draw data values.} 
    \item{\code{evalEvidence}}{\code{(model, isubj, iocc, theta, data,
	covar=NULL)}: Calculate likelihood of data values. } 
    \item{\code{as_longform}}{\code{(x =
	"POMDP",n=nsubj(x),maxocc=maxocc(x), 
        minocc=minocc(x),weightType="all",
        name=deparse(substitute(x)))}: combines long form
      information from component models. (see \code{\link{as_longform}})}
    \item{mstep}{\code{(obj = "POMDP", data, \ldots, its=3,
	control=list(), workers=Workers$new(nmodels(obj))
      )}: Runs the \code{mstep} algorithm, see \code{\link{mstep}}.} 
  }
}
\author{Russell Almond}
\note{

  Only the evidence models are needed for \code{\link{qFilter}}.  All
  models are needed for the other functions.

}
\seealso{

  \code{\linkS4class{Population}}, \code{\linkS4class{Activities}},
  \code{\link{Evidence}}. 

  \code{\link{nsubj}}, \code{\link{drawData}},
  \code{\link{as_longform}}, \code{\link{mstep}}

}
\examples{
showClass("POMDP")
}
\keyword{classes}
