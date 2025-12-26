\name{expLambdaT}
\alias{expLambdaT}
\title{Calculates a matrix exponential.}
\description{

  This function calculates the exponential of a matrix.  It is used by
  \code{\linkS4class{TransitionModel}} to covert a rate matrix to a
  transition matrix.

}
\usage{
expLambdaT(Lambda, nmoments = 10)
}
\arguments{
  \item{Lambda}{A square matrix to be exponentiated.}
  \item{nmoments}{An integer giving the number of times to square the
    matrix.  Higher numbers give a better approximation at the cost of
    longer run time.  Usually taken from the \code{$nmoments} field of
    the \code{\linkS4class{TransitionModel}}.}
}
\details{

  The matrix exponential can be calculated using the following
  relationship:
  
  \deqn{e^{\Lambda \Delta t} = \lim_{n \rightarrow \infty} \left ( I +
    \Lambda \frac{\Delta t}{n} \right )^n .}

  Raising the matrix to the \eqn{n}th power is more efficient if
  \eqn{n=2^m}.  The \code{nmoments} argument is the value of \eqn{m}. 

}
\value{

  A square matrix of the same size as the first argument.
  
}
\references{

  The expression for the matrix exponentiation comes from

  Ross, Sheldon (2023). \emph{Introduction to Probability
  Models.} \url{https://shop.elsevier.com/books/introduction-to-probability-models/ross/978-0-443-18761-2}

}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{TransitionModel}}, \code{\link{memoTree}}
}
\examples{

  amat <- matrix(c(-1,1,0, 1,-2,1, 0,1,-1)/2,3,3)
  expLambdaT(amat)

}
\keyword{ matrix }
\concept{ exponential }

