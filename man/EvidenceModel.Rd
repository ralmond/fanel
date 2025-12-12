\name{EvidenceModel}
\Rdversion{1.1}
\docType{class}
\alias{EvidenceModel-class}
\alias{EvidenceModel}

\title{Class \code{"EvidenceModel"}}
\description{

  This abstract class represents the likelihood of an observation given
  the value of the latent variable.

}
\section{Slots and Active Values}{

  \describe{
    \item{\code{$convergence}:}{Logical variable indicating whether or
      not the last \code{mstep} converged.}
    \item{\code{$lp}:}{The log-posterior after the last \code{mstep}.}
    \item{\code{$name}:}{A name for the model, primarily used for printing.}
    \item{\code{$wname}:}{The name of the weight column(s). }
    \item{\code{$tnames}:}{The name(s) of the latent variable(s).}
    \item{\code{$pvec}:}{A vector of possibly transformed parameters
      used in \code{mstep}.}
  }
}

\section{Methods}{

  Many of these need to be overridden in the implementing classes.

  \describe{
    \item{\code{$drawObs}}{\code{signature(theta,covars=list())}:  Draws a random
      response.}
    \item{\code{$llike}}{\code{signature(Y,theta,context=list())}: Calculates the log
      likelihood of the observations given the latent variable and
      current parameters.}
    \item{\code{$lprob}}{\code{signature(data,par=self$pvec)}: Calculated the
      log likelihood of the observations given estimated theta and
      argument parameters.} 
    \item{\code{$mstep}}{\code{signature(data,...,
	its=3,control=list())}:  Runs the optimizer for \code{its} steps.}
    \item{\code{$print}}{\code{signature(...)}: Prints the object.}
    \item{\code{$toString}}{\code{signature(...)}: Creates a string
      represenation of the object.}
  }

  The arguments to these functions are as follows:
  \describe{
    \item{theta}{The value of the latent variable, possibly a vector
      over particles.}
    \item{covar}{A list of additional measurement-occassion-subject
      specific covariates.}
    \item{Y}{The observed value.}
    \item{par}{The vectorized parameter.}
    \item{weights}{The weights for the particles.}
    \item{its}{The number of iterations of the optimizer to run.}
    \item{control}{Other control parameters passed to
      \code{\link{optim}}.}
  }

}
\section{Superclasses and Subclasses}{

  This is a subclass of \code{\linkS4class{FModel}}.

  Generally, a subclass needs to implment the \code{$pvec} active field
  and the \code{$drawObs}, \code{$lprob}, \code{$llike}, \code{$toString} and
  \code{$initialize} methods.

  Currently implemented subclasses include:
  \code{\linkS4class{GradedResponse}}, \code{\linkS4class{NormalScore}}

}

\author{Russell G. Almond}
\seealso{

  \code{\linkS4class{GradedResponse}}, \code{\linkS4class{NormalScore}},
  \code{\linkS4class{Evidence}}

}
\examples{
MyNormalScore <- R6Class(
    classname="MyNormalScore",
    inherit=EvidenceModel,
    public=list(
        initialize = function(name, bias, se) {
          self$name <- name
          self$bias <- bias
          self$se <- se
        },
        bias=0,
        se=01,
        draw = function(theta,context=list()) {
          rnorm(length(theta),theta+self$bias,self$se)
        },
        llike = function(Y,theta,context=list()) {
          dnorm(Y,theta+self$bias,self$se,log=TRUE)
        },
        lprob = function(data,par=self$pvec) {
          Y <- data[[self$dnames]]
          theta <- data[[self$tnames]]
          weights <- data[[self$wnames]]
          sum(dnorm(Y,theta+par[1],exp(par[2]),log=TRUE)*weights)
        },
        mstep = function(data,...) {
          weights <- data[[self$wname]]
          theta <- data[[self$tnames]]
          Y <- data[[self$dnames]]
          self$bias <- wtd.mean(Y,weights) - wtd.mean(theta,weights)
          self$se <- wtd.sd(Y,weights)
          self$converged <- true
          self$lp <- self$lprob(data)
          list(name=self$name,list(self$bias,self$se))
        },
        toString=function(digits=2,...){
          paste0("<NS: ", self$name, " ( ",
                 round(self$bias,digits=digits),
                 ", ",paste(round(self$se,digits=digits),
                            collapse = ", "), " )>")
        }

    ),
    active=list(
        pvec = function(value) {
          if (missing(value)) return(c(self$bias,log(self$se)))
          self$bias <- value[1]
          self$se <- exp(value[2])
        }
    )
)


}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{ POMDP}
\concept{ HMM}
\concept{evidence_model}
