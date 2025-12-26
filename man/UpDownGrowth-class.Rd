\name{UpDownGrowth-class}
\docType{class}
\alias{UpDownGrowth-class}
\alias{UpDownGrowth}

\title{Class \code{"UpDownGrowth"}}
\description{

  This is a \code{\linkS4class{TransitionModel}} which specifies a
  transition matrix through the increase (\dQuote{up}) rate and the
  decrease (\dQuote{down}) rate.  This is a Birth and Death process. 

}
\section{Public and Active Fields}{

  Local to this class:
  \describe{
    \item{\code{$uprate}}{The rate at which the latent variable
      advances. This can be a scalar or vector of size one less than
      \code{$nStates}.}
    \item{\code{$downrate}}{The rate at which the latent variable
      retreats. This can be a scalar or vector of size one less than
      \code{$nStates}.}
    \item{\code{$nStates}}{The number of possible states for the latent
      variables.}
    \item{\code{$pvec}:}{\code{log(c(self$uprate,self$downrate)))}.
      Note that if the length is greater than 2, it is assumed that the
      uprate has length \code{$nStates-1}.}
  }

  Inherited:
  \describe{
    \item{\code{$wname}}{The names of the weight columns.  This is
      expected to be of length 3 with the first one being the overall
      weight, the second the left weights and the third the right
      weights (see \code{\linkS4class{BWQuad}}).}
    \item{\code{$convergence}:}{Logical variable indicating whether or
      not the last \code{mstep} converged.}
    \item{\code{$lp}:}{The log-posterior after the last \code{mstep}.}
    \item{\code{$name}:}{A name for the model, primarily used for printing.}
    \item{\code{$wname}:}{The name of the weight column(s). }
    \item{\code{$tnames}:}{The name(s) of the latent variable(s).}
    \item{\code{$dosname}}{The name of the dosage column in the
      \code{data} for \code{mstep}, defaults to \dQuote{dose}.}
    \item{\code{$dtname}}{The name of the delta time column in the
      \code{data} for \code{mstep}, defaults to \dQuote{deltaT}.}
    \item{\code{$continuous}}{A logical value, true for models designed
      to be used with the \code{\link{particleFilter}}, and false for
      \code{\linkS4class{TransitionModel}}.}
    \item{\code{$nmoments}}{Number of times to exponentiate matrix for
      \code{\link{expLambdaT}}.}
    \item{\code{$splitter}}{An expression giving the variables which
      influence matrix creation.  Defaults to \code{~deltaT+dose}.}
    \item{\code{$cache}}{A \code{\linkS4class{memoTree}} used to cache
      matrixes.}
  }
}
\section{Methods}{

  \describe{
    \item{\code{$initialize}}{\code{(name, nStates, uprate, downrate,
	tname="theta", wname=c("w.full", "w.left", "w.right"))}:
      Called by \code{$new}}
    \item{\code{$rmat}}{\code{(pvec=pvec(self), deltaT, dose=deltaT,
	covar=list())}:  Calculates the rate matrix.  The \code{$uprate}
      is put on the superdiagonal, the \code{$downrate} on the
      subdiagonal, and the diagonal is set so the rows sums are zero.}
  }

  Inherited methods:
    \describe{
    \item{\code{$tmat}}{\code{(pvec=pvec(self), deltaT, dose=deltaT,
	covars=list())}: This calculateds the transition matrix (output
      from \code{\link{expLambdaT}}).}
    \item{\code{$advance}}{\code{(lweights, deltaT, dose=deltaT,
	covars=list())}: Multiplies tmat by lweights.  (See
      \code{\link{bwFilter}}.)}
    \item{\code{$retreat}}{\code{(rweights, deltaT, dose=deltaT,
	covars=list())}: Multiplies rweights by tmat. (See
      \code{\link{bwFilter}}.)}
    \item{\code{$lpinner}}{\code{(data,par=self(pvec))}:  An inner
      calculation for \code{$lprob}.}
    \item{\code{$fillCache}}{\code{(data,par=self(pvec))}:  Clears the
      cache and precalculates the matrixes needed for \code{data}.}
     \item{mstep}{\code{(obj = "FModel", data= "data.frame", ...)}:
       In this case a closed form maximum is available.}
    \item{\code{$mstep}}{\code{(data, ...)}:  This method does the
      optimization.}
    \item{lprob}{\code{(obj = "FModel", data= "data.frame",
	pvec = "numeric",)}:
      Calculates the log probability of the data (which includes imputed
      values for the latent variables and weights).}
    \item{\code{$lprob}}{\code{(data,pvec=pvec(self))}:  The R6 method
      which implement this.}
    \item{\code{$drawNext}}{\code{signature(theta,deltaT,dose=deltaT,
	covars=list())}: Draws a random next value for the latent
      variables.}
    \item{\code{$lprob}}{\code{signature(data,par=pvec(self))}:
      Calculated the log likelihood of the estimated growth given
      argument parameters.}
    \item{\code{$print}}{\code{signature(...)}: Prints the object.}
    \item{\code{$toString}}{\code{signature(...)}: Creates a string
      represenation of the object.}
  }
}
\section{Constructor}{

To create a new object of this type call:
\code{UpDownGrowth$new(\var{name}, \var{nStates}, \var{uprate},
  \var{downrate}, \var{tname}="theta", \var{wname}=c("w.full",
  "w.left", "w.right"))}.

}
\section{Superclasses and Subclasses}{

  Subclass of \code{\linkS4class{TransitionModel}}, which is in turn a
  subclass of \code{\linkS4class{GrowthModel}} and
  \code{\linkS4class{FModel}}.  

}
\references{

  Ross, Sheldon (2023). \emph{Introduction to Probability
  Models.} \url{https://shop.elsevier.com/books/introduction-to-probability-models/ross/978-0-443-18761-2}

  Almond, Russell (2025).  A Partially Observed Markov Decision Process for
Learning Progressions.  Paper presented at the Florida Educational
Research Association (FERA).  \url{https://ralmond.net/LearnDP}.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{TransitionModel}}, \code{\linkS4class{ActivitiesD}}

}
\examples{

udm <- UpDownModel$new("Activity1",3,c(.2,.1),.01)
udm
pvec(udm)
udm$rmat(deltaT=1,dose=.5)
udm$tmat(deltaT=1,dose=.5)

}
\keyword{classes}
