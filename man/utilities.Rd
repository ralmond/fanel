\name{cuts2probs}
\alias{utilties}
\alias{cuts2probs}
\alias{logit}
\alias{invlogit}
\title{Some useful calculation functions.}
\description{

  Internal utility functions which may be of some use.
  
}
\usage{
cuts2probs(cuts)
logit(p)
invlogit(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cuts}{The probability at being in a particular category or
    above.}
  \item{p}{A value between 0 and 1.}
  \item{x}{A real value.}
}
\details{

  The \code{cuts2probs} function is a helper function for the
  \code{\linkS4class{GradedResponse}} model.

  The \code{logit} and \code{invlogit} functions are the logistic and
  inverse logistic functions.
  
}
\value{

  The \code{cuts2probs} function returns a probability vector of length
  one more than the cuts.

  The \code{logit} and \code{invlogit} return a real number and a
  probability respectively.

}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{GradedResponse}}
}
\examples{

cuts2probs(c(.7,.5,.1))
logit(.5)
invlogit(1)

}
\keyword{ utilities }
\concept{ IRT }
