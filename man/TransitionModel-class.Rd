\name{TransitionModel-class}
\docType{class}
\alias{TransitionModel-class}
\alias{TransitionModel}

\title{Class \code{"TransitionModel"}}
\description{

  This is an extension of the \code{\linkS4class{GrowthModel}} class for
  use with the \code{\link{bwFilter}}.  In particular, it supports
  building a transition matrix which looks at the probability of the
  subject changing the latent state across the time interval.

}
\section{Public and Active Fields}{

  Inhereted from \code{\link{GrowthModel}}:

  \describe{
    \item{\code{$convergence}:}{Logical variable indicating whether or
      not the last \code{mstep} converged.}
    \item{\code{$lp}:}{The log-posterior after the last \code{mstep}.}
    \item{\code{$name}:}{A name for the model, primarily used for printing.}
    \item{\code{$wname}}{The names of the weight columns.  This is
      expected to be of length 3 with the first one being the overall
      weight, the second the left weights and the third the right
      weights (see \code{\linkS4class{BWQuad}}).}
    \item{\code{$tnames}:}{The name(s) of the latent variable(s).}
    \item{\code{$pvec}:}{A vector of possibly transformed parameters
      used in \code{mstep}.}
    \item{\code{$dosname}}{The name of the dosage column in the
      \code{data} for \code{mstep}, defaults to \dQuote{dose}.}
    \item{\code{$dtname}}{The name of the delta time column in the
      \code{data} for \code{mstep}, defaults to \dQuote{deltaT}.}
    \item{\code{$continuous}}{A logical value, true for models designed
      to be used with the \code{\link{particleFilter}}, and false for
      \code{\linkS4class{TransitionModel}}.}
  }

  Unique to \code{TransitionModel}s:

  \describe{
    \item{\code{$nStates}}{The number of rows and columns in the matrix.}
    \item{\code{$nmoments}}{Number of times to exponentiate matrix for
      \code{\link{expLambdaT}}.}
    \item{\code{$splitter}}{An expression giving the variables which
      influence matrix creation.  Defaults to \code{~deltaT+dose}.}
    \item{\code{$cache}}{A \code{\linkS4class{memoTree}} used to cache
      matrixes.}
  }
  
}
\section{Methods}{

  Interited from \code{\linkS4class{GrowthModel}}.
  \describe{
     \item{mstep}{\code{(obj = "FModel", data= "data.frame", its="integer",
	control="list", workers = "Workers")}:  Runs the m-step to
      optimize parameters (inherited from \code{\linkS4class{FModel}}).}
    \item{\code{$mstep}}{\code{(data, ..., its=3,control=list(),
	workers = NULL)}:  This method does the optimization (see
      \code{\linkS4class{FModel}}).}
    \item{lprob}{\code{(obj = "FModel", data= "data.frame",
	pvec = "numeric",)}:
      Calculates the log probability of the data (which includes imputed
      values for the latent variables and weights).}
    \item{\code{$lprob}}{\code{(data,pvec=pvec(self))}:  The R6 method
      which implement this.}
    \item{pvec}{\code{(obj = "FModel")}: Returns the
      parameters as a vector.  The actual value depends on the
      particular model class. }
    \item{pvec<-}{\code{(obj = "FModel", value)}: Sets the parameter
      vector. }
    \item{\code{$drawNext}}{\code{signature(theta,deltaT,dose=deltaT,
	covars=list())}: Draws a random next value for the latent
      variables.}
    \item{\code{$lprob}}{\code{signature(data,par=pvec(self))}:
      Calculated the log likelihood of the estimated growth given
      argument parameters.} 
    \item{\code{$print}}{\code{signature(...)}: Prints the object.}
    \item{\code{$toString}}{\code{signature(...)}: Creates a string
      represenation of the object.}
    \item{\code{$xtime}}{A character vector giving the name of
      additional covariates used in building the matrix.}
  }

  The arguments to these functions are as follows:
  \describe{
    \item{theta}{The value of the latent variable at the initial time.}
    \item{deltaT}{The time elapsed between the previous and current
      occasion.} 
    \item{dose}{The amount to which the activity was done between the
      previous and current  occasion.} 
    \item{covars}{A data-frame containing covariates at the current
      occasion.} 
    \item{par}{The vectorized parameter.}
    \item{data}{A combined data set which contains columns for thetas
      and weights.}
    \item{...}{Additional arguments for \code{\link{mstep}}.}
    \item{its}{The number of iterations of the optimizer to run.}
    \item{control}{Other control parameters passed to
      \code{\link{optim}}.}
  }

  Methods unique to \code{TransitionModel}:

  \describe{
    \item{\code{$rmat}}{\code{(pvec=pvec(self), deltaT, dose=deltaT,
	covars=list())}: This calculateds the rate matrix (input to
      \code{\link{expLambdaT}}).}
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
  }

  The arguments to these functions are as follows:
  \describe{
    \item{theta}{The value of the latent variable at the initial time.}
    \item{deltaT}{The time elapsed between the previous and current
      occasion.} 
    \item{dose}{The amount to which the activity was done between the
      previous and current  occasion.} 
    \item{covars}{A data-frame containing covariates at the current
      occasion.} 
    \item{par}{The vectorized parameter.}
    \item{data}{A combined data set which contains columns for thetas
      and weights.}
    \item{...}{Additional arguments for \code{\link{mstep}}.}
    \item{its}{The number of iterations of the optimizer to run.}
    \item{control}{Other control parameters passed to
      \code{\link{optim}}.}
    \item{lweights,rweights}{The left and right weights from the
      \code{\link{BWQuad}}.}
  }

}
\section{Superclasses and Subclasses}{

  Its superclass are \code{\linkS4class{FModel}} and
  \code{\linkS4class{GrowthModel}}. 

  Generally, a subclass needs to implment the \code{$pvec} active field
  and the \code{$rmat} and \code{$initialize} methods.  If covariates
  are needed then the values of \code{$splitter} and \code{$xtime} will
  need to be adjusted.

  Existing implementations are:
  \code{\linkS4class{UpDownGrowth}}.

}
\section{Building the Transistion Matrix}{

  Conceptually, the transion matrix is defined by defining a rate
  matrix and multiplying it by the time, \eqn{\Lambda\Delta t}.  This
  is calculated through the \code{$rmat()} method.  This is then
  exponentiated:
  \deqn{e^{\Lambda \Delta t} = \lim_{n \rightarrow \infty} \left ( I +
    \Lambda \frac{\Delta t}{n} \right )^n .}

  The function \code{\link{expLambdaT}} does this exponentiation.
  Raising the matrix to the \eqn{n}th power is more efficient if
  \eqn{n=2^m}.  Here \code{$nmoments} is the value of \eqn{m}.

  The \code{$tmat()} method builds the final transition matrix.  It
  calls \code{$rmat()} to generate the rate matrix and
  \code{\link{expLambdaT}} to do the exponentiation.  As this is an
  expensive calculation, the results are cached (see next section).  In
  general, subclasses only need to implement an \code{$rmat()} function
  and can use the default \code{$tmat()}.

}
\section{Caching matrixes}{

  As \code{\link{expLambdaT}} is expensive, the \code{TransitionModel}
  caches the results.  The key is the time vector, which consists of
  \code{deltaT}, \code{dose} and any additional variables named in
  \code{$xtimes}. The cache is cleared when the parameters are changed.

  When scoring a large number of subjects using
  \code{\linkS4class{Workers}}, the worker threads will not share the
  cache.  Therefore, the cache can be pre-filled by using the
  \code{$fillCache} function.  This function is passed the data set
  (from the last \code{\link{mstep}} to get the time vectors for which
  calculations are needed.

  The \code{$lprob()} method takes a different approach, it splits the
  data matrix according to \code{$splitter}, which corresponds to the
  cache keys.  The function \code{$lpinner} is then run using only the
  data which has a common time vector.

}
\author{Russell G. Almond}
\note{

  The transition model really works best with a single categorical
  latent variable.  If this variable has \eqn{k} possible states, and an
  potential giving the value of the latent variable is a vector of
  length \eqn{k}.  Then the transition matrix is a \eqn{k \times k}
  matrix. This means that the value of \code{$nStates} must be
  co-ordinated between the transition models and the
  \code{\linkS4class{BWQuad}}.

  Things are more difficult when the latent variable is
  multidimensional.  The potential is now a \eqn{k_1 \times k_2 \times
  \ldots} tensor, which has been linearized to a vector of length
  \eqn{K= k_1 \cdot k_2 \cdots}.  The transition matrix is a \eqn{k_1
    \times k_2 \times \ldots \timesk_1 \times k_2 \times \ldots} tensor
  which has been collapsed into a \eqn{K \times K} matrix.  I think this
  works but it makes my head hurt to think about it too long.

}
\seealso{

  \code{\linkS4class{POMDP}}, \code{\linkS4class{ActivitiesD}}

  \code{\linkS4class{FModel}}, \code{\linkS4class{GrowthModel}}

  \code{\linkS4class{UpDownGrowth}}

  \code{\link{expLambdaT}}, \code{\link{bwFilter}},
  \code{\link{BWQuad}}. 
}
\examples{
MyUpDown <- R6Class(
  classname="MyUpDown",
  inherit=TransitionModel,
  public=list(
    initialize = function(name, nStates, uprate, downrate,
                          tname="theta",
                          wname=c("w.full", "w.left", "w.right")) {
      self$name <- name
      self$nStates <- nStates
      self$uprate <- uprate
      self$downrate <- downrate
      self$tnames <- tname
      self$wname <- wname
    },
    uprate=0,
    downrate=0,
    rmat = function(pvec=pvec(self),deltaT,dose=deltaT,covar) {
      up <- exp(pvec[1:(self$nStates-1)])
      down <- exp(pvec[self$nStates:length(pvec)])
      matR <- matrix(0,self$nStates,self$nStates)
      mgcv::sdiag(matR,1) <- up*dose
      mgcv::sdiag(matR,-1) <- down*deltaT
      diag(matR) <- -rowSums(matR)
      matR
    },
    toString=function(digits=2,...) {
      paste0("<UpDownGrowth: ", self$name, " ( ",
             paste(round(self$uprate,digits=digits),
                   collapse = ", "),
             "; ",
             paste(round(self$downrate,digits=digits),
                   collapse= ", "),
             " )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(log(c(self$uprate,self$downrate)))
      if (length(value) == 2L) {
        self$uprate <- exp(value[1])
        self$downrate <- exp(value[2])
      } else {
        self$uprate <- exp(value[1:(self$nStates-1)])
        self$downrate <- exp(value[self$nStates:length(value)])
      }
    }
  )
)

}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{growth_model}
\concept{ POMDP}
\concept{ HMM}
\concept{ Baum-Welch}

