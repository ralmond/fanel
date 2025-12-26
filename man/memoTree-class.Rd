\name{memoTree-class}
\docType{class}
\alias{memoTree-class}
\alias{memoTree}

\title{Class \code{"memoTree"}}
\description{

  A cache for pre-calculated matrixes (see
  \code{\linkS4class{TransitionModel}}).
  
}
\section{Constructor}{
  \code{memoTree$new(depth=1L)}, where \code{depth} inidicates the
  length of the key used with this memoTree.
}
\section{Public and Private Fields}{
  Public Fields:
  \describe{
    \item{\code{depth}:}{Integer giving number of layers below.}
  }
  Private Fields:
  \describe{
    \item{\code{cache}:}{An environment used to cache the values.}
  }
}
\section{Extends}{
Class \code{\link{R6Class}}, directly.
}
\section{Methods}{

  \describe{
    \item{\code{$initialize}}{\code{(depth=1L)}: Called by \code{$new} .}
    \item{\code{$exists}}{\code{(key)}: Returns true if key exists and
      false otherwise.}
    \item{\code{$get}}{\code{(key)}: Returns the value if it is in the
      cache and \code{NULL} otherwise.}
    \item{\code{$assign}}{\code{(key,value)}: Assigns a value to the key
      in the cache.}
    \item{\code{$clear}}{\code{()}: Clears the cached values.}
  }

  Note that the \code{key} argument is a vector, and is converted to a
  character.
}
\details{

  This is typically used to store cached matrixes for
  \code{\linkS4class{TransitionModel}}s, which are expensive to compute.  
  The \code{key} is assumed to be a vector, and in the typical case, it
  consists of \code{c(deltaT, dose)}.  (It is possible to use longer
  keys if additional covariates are involved in the calculations).  The
  stored value is the transition matrix corresponding to the parameters.
  When the \code{TransitionModel$tmat()} method is called it first
  checks the cache to see if the matrix has already been built, and if
  not it builds it.

  If the \code{key} has length 1 (i.e, the \code{memoTree} has depth 1),
  then the value is stored directly.  If the \code{key} has length
  greater than one, then the a new \code{memoTree} with one smaller
  depth is associated with the first element of the key.  The remainder
  of the key is passed to the inner function.
  
}
\author{Russell Almond}
\note{

  This is a generalization of the \dQuote{memoization} design pattern.
}
\seealso{
  
  \code{\linkS4class{TransitionModel}}

}
\examples{

  cacheA <- memoTree$new(2)

  cacheA$assign(c(1,2), "One-Two")
  cacheA$assign(c(2,1), "Two-One")
  cacheA$exists(c(1,1))
  cacheA$exists(c(2,1))
  cacheA$get(c(1,2))
  cacheA$clear()
  cacheA$exists(c(1,2))

}
\keyword{classes}
\keyword{utilities}
