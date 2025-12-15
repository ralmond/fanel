\name{BrownianGrowth}
\Rdversion{1.1}
\docType{class}
\alias{BrownianGrowth-class}
\alias{BrownianGrowth}

\title{Class \code{"BrownianGrowth"}}
\description{

  A non-stationary Brownian motion process, where the gain and the
  variance are linear with the elapsed time, \eqn{\Delta t} or
  \code{deltaT}, and the dosage, \eqn{\tau} or \code{dose}.

  \deqn{\theta(t+1) \sim N(\theta(t) + b \tau, \sigma \sqrt{\Delta
      t}),}
  where \eqn{b} is the \code{gain} and \eqn{\sigma} is the
  \code{inovSD}.

}
\section{Public and Active Fields}{

  Local to this class:
  \describe{
    \item{\code{$gain}}{The rate at which the latent variable advances.}
    \item{\code{$inovSD}}{The rate at which the variance of the
      innovation varies with time.}
    \item{\code{$pvec}}{The parameter vector, \code{c(self$gain,
	log(self$inovSD))}.}
  }

  Inherited:
  \describe{
    \item{\code{$convergence}:}{Logical variable indicating whether or
      not the last \code{mstep} converged.}
    \item{\code{$lp}:}{The log-posterior after the last \code{mstep}.}
    \item{\code{$name}:}{A name for the model, primarily used for printing.}
    \item{\code{$wname}:}{The name of the weight column(s). }
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
}
\section{Methods}{

  \describe{
    \item{initialize}{\code{signature(name,gain,inovSD)}:  Called by
      \code{$new}.}
  }

  Inherited methods:
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
  }

}
\section{Constructor}{

To create a new object of this type call:
\code{BrownianGrowth$new(\var{name},\var{gain},\var{inovSD})}

}
\section{Superclasses and Subclasses}{

  Subclass of \code{\linkS4class{GrowthModel}}, which is in turn a
  subclass of \code{\linkS4class{FModel}}.

}
\author{Russell G. Almond}
\seealso{

  \code{\linkS4class{GrowthModel}}, \code{\linkS4class{Activities}}
  \code{\linkS4class{FModel}}

}

\examples{

bgm <- BrownianGrowth$new("amodel",.1,.05)
bgm
pvec(bgm)
bgm$drawNext(c(-1,0,1),10,1)
bdat <- data.frame(theta=c(-1,0,1),theta_1=c(-.9,.05,.9),
                    w=c(.4,.3,.3),deltaT=1,dose=1)
bgm$lprob(bdat,bgm$pvec)


}
\keyword{classes}
