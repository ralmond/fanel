\name{bwFilter}
\alias{bwFilter}
\title{Runs the Baum-Welch Filter on a POMDP Model and Data}
\description{

  The Baum-Welch filter is designed to work with a categorical latent
  variable, where the \code{\linkS4class{TransitionModel}}s can be
  described through transition matrixes.  This filter is designed to
  work with a \code{\linkS4class{POMDP}} where the activities are an
  \code{\linkS4class{ActivitiesD}} and return a
  \code{\linkS4class{BWQuad}}.

}
\usage{
bwFilter(object, covars, quad, ..., workers = Workers$new())
}
\arguments{
  \item{object}{A \code{\linkS4class{POMDP}} giving the information
    about the population, growth models and evidence.}
  \item{covars}{A \code{\linkS4class{Panel_Data}} which has the data and
    any other needed covariates.}
  \item{quad}{A \code{\linkS4class{BWQuad}} to store the results.}
  \item{\dots}{Separates required from optional parameters.}
  \item{workers}{A \code{\link{Workers}} object to control parallel
    processing.}
}
\details{

  The Baum-Welsch algorithm is based on a discrete latent variable,
  where each quadrature point represents a possible state of the latent
  variable. It then works in two directions:  forward and back.

  In the forward phase, initial weights are assigned based on calling 
  \code{\link{probInit}} on the \code{\linkS4class{Population}}.  For
  each observation/growth period, the weights are updated using
  \code{\link{advanceWeights}} and the result is multiplied by the value
  from \code{exp(\link{evalEvidence}())}.  This produces the
  \dQuote{left} weights.

  In the backward phase, the initial weights are set to 1, at each stage
  the weights are multiplied by the likelihood
  (\code{exp(evalEvidence())}) and then multiplied on the other side by
  the transition matrix (\code{\link{retreatWeights}}).  This produces
  the \dQuote{right} weights.

  The separate left and right weights are needed for the
  \code{\link{mstep}} of the EM algorithm on the
  \code{\linkS4class{TransitionModel}}.  (Note that the \code{$lweights}
  and \code{$rweights} of the \code{\linkS4class{BWQuad}} are on the
  natural scale unlike the other quadratures in which \code{$lweights}
  are on the log scale.)  The \dQuote{full} weights are created by
  multiplying the left and right weights.

}
\value{

  A \code{\linkS4class{BWQuad}} object containing the quadrature points
  and the left and right weights.

}
\section{Parallel Processing}{
  
  The work is separated by subject, and each subject is scored
  independently.  These are split among the \code{workers}.
  Turning on the debug option in the
  workers argument (\code{\linkS4class{Workers}$new(debug=TRUE)}) will
  run all in a single thread which makes debugging easier.

}
\references{

Almond, R. G. (2025). A Partially Observed Markov Decision Process for
Learning Progressions. Annual Meeting of the Florida Educational
Research Association, Ft. Myers, Florida. https://ralmond.net/LearnDP
  
[Baum-Welch Algorithm
(Wikipedia)](https://en.wikipedia.org/wiki/Baum%E2%80%93Welch_algorithm) 

}
\author{Russell Almond}
\note{

  The Baum-Welch algorithm assumes that the latent state is a single
  categorical variable.  It is probably sensible to think about applying
  the algorithm to a categorical variable which represents quadrature
  points for an underlying continuous variable, however, this will
  probably require a new kind of transition model.

  For a \eqn{K}-dimensional discrete latent variable, the weights are
  actually \eqn{K}-dimensional tensors unrolled to vectors.  The
  transition \dQuote{matrix} is actually a \eqn{K\cdot K}-dimensional
  tensor flattend into a matrix.

  For a multidimensional continuous or mixed latent variable, I suggest
  giving up and using the \code{\link{particleFilter}}.
  

}
\seealso{

  \code{\linkS4class{POMDP}}, \code{\linkS4class{ActivitiesD}},
  \code{\linkS4class{TransitionModel}},
  \code{\linkS4class{Panel_Data}}, \code{\linkS4class{BWQuad}},
  \code{\linkS4class{Workers}}, 
  \code{\link{evalEvidence}}, \code{\link{probInit}},
  \code{\link{advanceWeights}}, \code{\link{retreatWeights}}  

}
\examples{
}
\keyword{ ts }
\concept{ panel_data }
\concept{ filter }
\concept{ Baum-Welch}
