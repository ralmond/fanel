\name{PopulationModel}
\Rdversion{1.1}
\docType{class}
\alias{PopulationModel-class}
\alias{PopulationModel}

\title{Class \code{"PopulationModel"}}
\description{

  An abstract class describing the initial distribution of a population.

}
\section{Public Fields}{
  All fields are inhereted from \code{\linkS4class{FModel}}.
  \describe{
    \item{\code{$name}:}{A name for the model, primarily used for printing.}
    \item{\code{$convergence}:}{Logical variable indicating whether or
      not the last \code{mstep} converged.}
    \item{\code{$lp}:}{The log-posterior after the last \code{mstep}.}
    \item{\code{$wname}:}{The name of the weight column(s). }
    \item{\code{$tnames}:}{The name(s) of the latent variable(s).}
  }
}
\section{Active Fields}{
  All fields are inhereted from \code{\linkS4class{FModel}}.
  \describe{
    \item{\code{$pvec}}{Returns a vectorized version of the parameters.}
  }
}

\section{Methods}{

  Local methods:
  \describe{
    \item{$drawInit}{\code{signature(npart,covars=list())}:
      Draws a random starting position. (Used with random quadratures,
      i.e., particle filter, and simulations.)}
    \item{$initProbs}{\code{signature(theta,covars=list())}:
      Calculates the initial probabilities for fixed quadratures.}
  }
  Note these methods must be implemented for subclasses.

  \describe{
    \item{\code{$lprob}}{\code{signature(data,par=self$pvec)}:
      Calculates the log probability of the the starting position.}
    \item{\code{$toString}}{\code{signature(...)}: Creates a string
      represenation of the object.}
    \item{\code{$print}}{\code{signature(...)}: Prints the object.}
    \item{\code{$mstep}}{\code{(data, ...)}:
	This method does the optimization (may need to be overridden). } 
  }

  The arguments to these functions are as follows:
  \describe{
    \item{npart}{The number of latent variables to draw.}
    \item{theta}{The value of the latent variable at the initial time.}
    \item{covars}{A data-frame containing covariates at occasion 0.}
    \item{par}{The vectorized parameter.}
    \item{data}{A combined data set which contains columns for thetas
      and weights.}
    \item{...}{Additional arguments for \code{\link{mstep}}.}
  }
}
\section{Constructor}{

  This is an abstract class, usually just used for subclasses.

}
\section{Superclasses and Subclasses}{

  This is an instance of \code{\link[R6]{R6Class}}.

  Generally, a subclass needs to implment the \code{$pvec} active field
  and the \code{$drawInit}, \code{$lprob}, \code{$initProbs},
  \code{$toString} and \code{$initialize} methods.

  Existing subclasses:  \code{\linkS4class{NormalPop}},
  \code{\linkS4class{CategoricalPop}}

}
\section{M-step}{

  The default m-step method runs the optimizer to iteratively find the
  maximum of the \code{$lprob} function.  However, in a number of cases,
  it might be possible to compute the maximum analytically.  In this
  case, the class should override the \code{$mstep} method.  It should
  return a list with the name of the model and some indication of the
  result. 

}
\author{Russell G. Almond}
\seealso{

  Currently implemented subclasses:
  \code{\linkS4class{NormalPop}},  \code{\linkS4class{CategoricalPop}}.

  Container class \code{\linkS4class{Population}}.

}
\examples{

MyNormalPop <- R6Class(
    classname = "MyNormalPop",
    inherit=PopulationModel,
    public=list(
        initialize = function(name,mu,sigma) {
          self$name <- name
          self$mu <- mu
          self$sigma <- sigma
        },
        mu=0,
        sigma=1,
        drawInit = function(npart,covars=list()) {
          mu <- self$mu
          sigma <- self$sigma
          rnorm(npart,mu,sigma)
        },
        initProbs = function(theta,covars=list()) {
          ord <- order(theta,decreasing=FALSE)
          ps <- pnorm(theta[ord],self$mu,self$sigma)
          pcuts <- c(0,ps[-1]-diff(ps)/2,1) #Midpoints of p's.
          diff(pcuts)[order(ord)] #order(ord) undoes the ordering.
        },
        lprob = function(data,par=self$pvec) {
          mu <- par[1]
          sigma <- exp(par[2])
          weights <- data[[self$wname]]
          theta <- data[[self$tnames]]
          sum(dnorm(theta,mu,sigma,log=TRUE)*weights)
        },
        mstep=function(data,...) {
          weights <- data[[self$wname]]
          theta <- data[[self$tnames]]
          self$mu <- wtd.mean(theta,weights)
          self$sigma <- wtd.sd(theta,weights)
          self$lp <- sum(dnorm(theta,self$mu,self$sigma,log=TRUE)*weights)
          self$convergence <- TRUE
          list(name=self$name,list(mu=self$mu,sigma=self$sigma))
        },
        toString=function(digits=2,...){
          paste0("<NormalPopulation: ",
          self$name, " ( ",round(self$mu,digits=digits),
                 ", ",round(self$sigma,digits=digits)," )>")
        }
    ),
    active=list(
        pvec = function(value) {
          if (missing(value)) return(c(self$mu,log(self$sigma)))
          self$mu <- value[1]
          self$sigma <- exp(value[2])
        }
    )
)

}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{population_model}
\concept{ POMDP}
\concept{ HMM}
