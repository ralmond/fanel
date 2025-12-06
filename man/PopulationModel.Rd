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
    \item{\code{$lprob}}{\code{signature(par=self$pvec,data)}:
      Calculates the log probability of the the starting position.}
    \item{\code{$print}}{\code{signature(...)}: Prints the object.}
    \item{\code{$toString}}{\code{signature(...)}: Creates a string
      represenation of the object.}
  }

  The arguments to these functions are as follows:
  \describe{
    \item{npart}{The number of latent variables to draw.}
    \item{theta}{The value of the latent variable at the initial time.}
    \item{covars}{A data-frame containing covariates at occasion 0.}
    \item{par}{The vectorized parameter.}
    \item{data}{A combined data set which contains columns for thetas
      and weights.}
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
        lprob = function(par=self$pvec,data) {
          mu <- par[1]
          sigma <- exp(par[2])
          weights <- data[[self$wname]]
          theta <- data[[self$tnames]]
          sum(dnorm(theta,mu,sigma,log=TRUE)*weights)
        },
        cdf = function(theta,invariants=list()) {
          pnorm(theta,self$mu,self$sigma)
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
