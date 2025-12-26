\name{particleFilter}
\alias{particleFilter}
\title{Runs the Particle Filter on a POMDP model}
\description{

  Simulates a series of trajectories through the latent space and
  weights them according to how likely they are to have generated the
  observed data.

}
\usage{
particleFilter(object, covars, quad = NULL, nquad = nquad(quad),
               ..., workers = Workers$new())
}
\arguments{
  \item{object}{A \code{\linkS4class{POMDP}} giving the information
    about the population, growth models and evidence.}
  \item{covars}{A \code{\linkS4class{Panel_Data}} which has the data and
    any other needed covariates.}
  \item{quad}{A \code{\linkS4class{ParticleQuad}} to store the results.
    If not supplied, one will be created.}
  \item{nquad}{The number of quadrature points to use.  Either
    \code{quad} or \code{nquad} must be supplied.}
  \item{\dots}{Separates required from optional parameters.}
  \item{workers}{A \code{\link{Workers}} object to control parallel
    processing.}
}
\details{

  The particle filter algorithm works generaing \code{nquad} simulated
  trajectories for the latent variable(s) using
  \code{\link{drawInitial}} and \code{\link{drawGrowth}}.  For each
  observation the log likelihood of that observation is calculated using
  \code{\link{evalEvidence}}.  (Note that the observation times should
  be a subset of times of the growth model in the
  \code{\linkS4class{POMDP}}.)  The estimate of the latent variables at
  any time point is a weighted estimate of the trajectories.

  The trajectories are calculated incrementally, and the
  \code{\linkS4class{ParticleQuad}} stores both the final weights
  (\code{quad$weights("default")}) and the incremental weights 
  (\code{quad$weights("history")}).  The default weights take both
  current and future observations into account, while the history
  weights only take observations prior to each time point into account.

}
\value{

  A \code{\linkS4class{ParticleQuad}} object containing the generated
  trajectories (the theta fields) and the weights.
  
}
\section{Parallel Processing}{
  
  The work is separated by subject, and each subject is scored
  independently.  These are split among the \code{workers}.
  Turning on the debug option in the
  workers argument (\code{\linkS4class{Workers}$new(debug=TRUE)}) will
  run all in a single thread which makes debugging easier.

}
\references{

  Almond, R. G. (2007). Cognitive Modeling to Represent Growth
  (Learning) Using Markov Decision Processes. \emph{Technology,
  Instruction, Cognition and Learning (TICL)}, \strong{5}, 313–324.


}
\author{Russell Almond}
\note{

  Sometimes particle filters do boostrap resampling to avoid problems
  where all of the mass concentrates on just a couple of particles.  The
  current implementation does not do that.

}
\seealso{

  \code{\linkS4class{POMDP}}, \code{\linkS4class{Evidence}},
  \code{\linkS4class{Panel_Data}}, \code{\linkS4class{ParticleQuad}},
  \code{\linkS4class{Workers}}, 
  \code{\link{evalEvidence}}, \code{\link{drawInitial}},
  \code{\link{drawGrowth}} 

}
\examples{
}
\keyword{ ts }
\concept{ panel_data }
\concept{ filter }
\concept{ particle_filter}
