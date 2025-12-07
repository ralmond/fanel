\name{NormalPop}
\Rdversion{1.1}
\docType{class}
\alias{NormalPop-class}
\alias{NormalPop}

\title{Class \code{"NormalPop"}}
\description{

  A normal distribution for initial values.

}
\section{Public Fields}{

  Local to this class.
  \describe{
    \item{\code{$mu}}{Population mean.}
    \item{\code{$sigma}}{Population standard deviation.}
  }

  Inherited fields:
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
  \describe{
    \item{\code{$pvec}}{The vector \code{c(mu,log(sigma))}.}
  }
}
\section{Methods}{

  \describe{
    \item{\code{$initialize}}{\code{(name,mu,sigma,tname,wname)}:
      Constructor, called by \code{$new}.}
    \item{\code{$drawInit}}{\code{(npart,covars=list())}:
      Draws a random starting position. (Used with random quadratures,
      i.e., particle filter, and simulations.)}
    \item{\code{$initProbs}}{\code{(theta,covars=list())}:
      Calculates the probability of the initial quadrature.}
    \item{\code{$lprob}}{\code{(data,par=self$pvec)}:
      Calculates the log probability of the the starting position.}
    }

}
\section{Constructor}{

The usual expression for building this distribution is
\code{NormalPop$new(\var{name},\var{mean},\var{sd},\var{tname}="theta",
  \var{wname}="w")}.

}
\section{Superclasses and Subclasses}{

  Is a subclass of
  \code{\linkS4class{PopulationModel}} which is in turn a subclass of
  \code{\linkS4class{FModel}}.

}
\author{Russell G. Almond}
\seealso{

  \code{\linkS4class{FModel}},
  \code{\linkS4class{PopulationModel}}, \code{\linkS4class{Population}}

}

\examples{
apop <- NormalPop$new("standard",0,1)
apop$pvec
apop$drawInit(3)
apop$lprob(data.frame(theta=c(-1,0,1),w=c(1,2,1)/4),c(0,0))
apop$initProbs(c(-1,0,1))

}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{ POMDP}
\concept{ HMM}
