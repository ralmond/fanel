\name{drawData}
\alias{drawData}
\alias{drawInitial}
\alias{drawGrowth}
\alias{evalEvidence}
\alias{advanceWeights}
\alias{retreatWeights}
\alias{probInit}
\title{Generic functions for filters}
\description{

  These are the core functions supplied by various models used to
  support \code{\link{simulate.POMDP}}, \code{\link{qFilter}},
  \code{\link{particleFilter}} and \code{\link{bwFilter}}.  

}
\usage{
drawInitial(model, isubj, npart, covar = NULL)
probInit(model, isubj, thetas, covar = NULL)
drawData(model, isubj, iocc, theta, covar = NULL)
evalEvidence(model, isubj, iocc, theta, data, covar = NULL)
drawGrowth(model, isubj, iocc, theta, covar = NULL)
advanceWeights(model, isubj, iocc, lweights, covar = NULL)
retreatWeights(model, isubj, iocc, rweights, covar = NULL)
}
\arguments{
  \item{model}{A \code{\linkS4class{ModelSet}} of the appropriate type
    or \code{\linkS4class{POMDP}}.}
  \item{isubj}{An integer giving the index of the subject.}
  \item{iocc}{An integer giving the index of the occasion.}
  \item{npart}{The number of random starting points to draw.}
  \item{theta,thetas}{A data frame of values for the latent variable.}
  \item{covar}{A \code{\linkS4class{Panel_Data}} object giving
    additional covariates.}
  \item{data}{A data frame containing data.}
  \item{lweights,rweights}{A vector of weights, corresponding to the
    size of the matrix.}
}
\details{

  All of the functions operate on a \code{\linkS4class{ModelSet}} (or a
  \code{\linkS4class{POMDP}} model which delegates to the appropriate
  model set.  They find the \code{\link{FModel}} corresponding to the
  given \code{isubj} and \code{iocc}, subset the \code{covar} argument
  according to the \code{isubj} and \code{iocc}, and pass along the rest
  of the arguments to the corresponding \code{FModel} method. 

  \tabular{lllll}{
    \strong{Function} \tab \strong{Model Set} \tab \strong{Calls} \cr
    \code{drawInitial} \tab \code{Population} \tab
    \code{PopulationModel$drawInit()} \cr
    \code{probInit} \tab \code{Population} \tab
    \code{PopulationModel$initProbs()} \cr
    \code{drawData} \tab \code{Evidence} \tab
    \code{EvidenceModel$drawObs()} \cr
    \code{evalEvidence} \tab \code{Population} \tab
    \code{EvidenceModel$llike()} \cr
    \code{drawGrowth} \tab \code{Activities} \tab
    \code{GrowthModel$drawNext()} \cr
    \code{advanceWeights} \tab \code{ActivitiesD} \tab
    \code{TransitionModel$advance()} \cr
    \code{retreatWeights} \tab \code{ActivitiesD} \tab
    \code{TransitionModel$retreat()} \cr
  }

  Note that the \code{\linkS4class{Activities}} class also finds the
  elapsed time (\code{deltaT}) and dosage (\code{dose}) associated with
  the subject and occasion and passes them into the \code{FModel}
  methods. 

  The \code{\link{simulate.POMDP}} function uses the \code{drawInitial},
  \code{drawGrowth}, and \code{drawData} functions.

  The \code{\link{qFilter}} uses the \code{evalEvidence}, and if present
  the \code{probInit} method.

  The \code{\link{particleFilter}} uses the \code{drawInitial},
  \code{drawGrowth}, and \code{evalEvidence} functions.

  The \code{\link{bwFilter}} uses the \code{probInit},
  \code{evalEvidence}, \code{advanceWeights} and \code{retreatWeights}
  functions. 
  
}
\value{

  Depends of the functions.

  \item{\code{drawInitial}}{A data frame or vector of random starting
    values.} 
  \item{\code{probInit}}{A vector of likelihoods for the initial values.}
  \item{\code{drawData}}{A data frame or vector of random data.}
  \item{\code{evalEvidence}}{A vector of likelihoods for the supplied
    data given the supplied latent variables.}
  \item{\code{drawGrowth}}{A data frame or vector of values for latent
    variables at the next time point.}
  \item{\code{advanceWeights}}{A vector of likelihoods corresponding to
    the latent variables at the next time point.}
  \item{\code{retreatWeights}}{A vectore lof likelihood corresponding to
    the latent variables at the previous time point.}
}
\author{Russell Almond}
\seealso{

  Model Sets:
  \code{\linkS4class{POMDP}}, \code{\linkS4class{ModelSet}},
  \code{\linkS4class{Population}}, \code{\linkS4class{EvidenceModel}},
  \code{\linkS4class{Activities}}, \code{\linkS4class{ActivitiesD}}.

  Abstract Models:
  \code{\linkS4class{FModel}}, \code{\linkS4class{PopulationModel}},
  \code{\linkS4class{EvidenceModel}}, \code{\linkS4class{GrowthModel}},
  \code{\linkS4class{TransitionModel}}.

  Data:
  \code{\linkS4class{Panel_Data}}.

  Simulation and Filters:
  \code{\link{simulate.POMDP}}, \code{\link{qFilter}},
  \code{\link{particleFilter}}, \code{\link{bwFilter}}.

}
\examples{
}
\keyword{models}
\concept{ panel_data }
\concept{population_model}
\concept{ POMDP}
\concept{ HMM}

