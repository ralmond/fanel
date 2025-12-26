\name{qFilter}
\alias{qFilter}
\title{A Generalized Likelihood Filter}
\description{

  This filters a latent variable time series by downweighting the
  likelihood of observations based on the distance in time between when
  the observation was made and the focus time at which the laten
  variable value is defined.  The \code{\linkS4class{FixedQuad}} gives
  the target times for the filter as well as the values of the latent
  variable at which the likelihood will be evaluated.  The \code{wfun}
  argument gives the \link{window} definition for the filter.

}
\usage{
qFilter(object, covars, quad, wfun, ..., workers = Workers$new())
}
\arguments{
  \item{object}{A \code{\linkS4class{POMDP}} or
    \code{\linkS4class{Evidence}} giving the information about the
    observation likelihoods.}
  \item{covars}{A \code{\linkS4class{Panel_Data}} which has the data and
    any other needed covariates.}
  \item{quad}{A \code{\linkS4class{FixedQuad}} which provides the times
    and quadrature points at which the latent variables will be
    estimated.}
  \item{wfun}{A function which provides the weighting \link{window}.
    Note that the window functions, like \code{\link{squareWindow}},
    return a function suitable for the \code{wfun} argument. See details.}
  \item{\dots}{A separator for optional arguments.}
  \item{workers}{A \code{\link{Workers}} object to control parallel
    processing.}
}
\details{

  The \code{\linkS4class{FixedQuad}} provides a number of quadrature
  points.  Let \eqn{\theta_q} be one of those points (not that theta
  could be multivariate).  It also defines a number of times at which
  estimates of the latent variables are desired, call these \eqn{t_{*}}.
  The input quadrature must specify both the values for the quadrature
  points and the time at which estimates are wanted (note that this does
  not need to be the same as the time points for which data are available).

  The data, \code{\linkS4class{Panel_Data}}, provide a number of
  observations, \eqn{Y_{ij}} for Subject \eqn{i} on Occasion \eqn{j}.
  Let \eqn{Q_{ij}(\theta) = \log P_{ij}(Y_{ij}| \theta)} be the log
  likelihood for the observation on Subject \eqn{i} at Time
  \eqn{t_{ij}}.  These likelihoods are provided through
  \code{evidence(\linkS4class{POMDP})}.  Note that the likelihood
  function can be different for each person--occasion pair.

  Let \eqn{w(t_{*},t_j)} be a weight function.  There are two kinds of
  filters.  The values of \eqn{t} such that \eqn{w(t_{*},t) > 0} define
  the window of the weight function.  A symmetric weight function has
  \eqn{w(t_{*},t_{*}-\tau) = w(t_{*},t_{*}+\tau)}, so its window is
  symmetric around \eqn{t_*}. This means future and past observations
  are both included in the estimate of the latent value.  An asymmetric
  weight function has \eqn{w(t_{*},t_{*}+\tau)=0} for \eqn{\tau > 0}.
  Thus, it only includes past measurement in estimating the latent
  variables.  The \link{window} function describes a number of functions
  for creating weight functions (e.g., \code{\link{gaussianWindow}}).
  Note that these all have \code{window} arguments which define the size
  of the window and \code{symmetric} arguments which define whether the
  window is symmetric.

  Given this setup, the combined log-likelihood for the latent variable
  is given by

  \deqn{Q_i(\theta,t_{*}) = \sum_j w(t_{*},t_j) Q_{ij}(\theta), }

  which is evaluated at the quadrature points \eqn{\theta_q}.

  Occasion 0 needs special treatment.  Often, there are no observations
  at occasion 0. There also may be information about the initial
  population distribution for the latent variable, \eqn{P_i(\theta)}.
  If the \code{population(\linkS4class{POMDP})} is supplied, then the
  likelihood equation becomes:
  
  \deqn{Q_i(\theta,t_{*}) = \sum_j w(t_{*},t_j) Q_{ij}(\theta) +
    w(t_{*},t_0)\log P_{i}(\theta).}

  The model only needs to specify the evidence component.  If the
  population component is specified, the second equation will be used.
  The activities are not used by this filter.


}
\value{

  Returns a \code{\linkS4class{FixedQuad}} with the weights populated.
}
\section{Parallel Processing}{
  
  The work is separated by subject, and each subject is scored
  independently.  These are split among the \code{workers}.
  Turning on the debug option in the
  workers argument (\code{\linkS4class{Workers}$new(debug=TRUE)}) will
  run all in a single thread which makes debugging easier.

}
\references{

Almond, R. G., Krumm, A. E., Kubie-Carpenter, B., Tajik, E., & Li,
J. (2025). Filters for IRT models with data collected across
time. Annual meeting of the National Council on Measurement in
Education. 

  
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{POMDP}}, \code{\linkS4class{Evidence}},
  \code{\linkS4class{Panel_Data}}, \code{\linkS4class{FixedQuad}},
  \code{\linkS4class{Workers}}, \code{\link{window}},
  \code{\link{evalEvidence}}, \code{\link{probInit}}
}
\examples{
}
\keyword{ ts }
\concept{ panel_data }
\concept{ filter }
