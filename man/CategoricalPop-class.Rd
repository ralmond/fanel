\name{CategoricalPop-class}
\docType{class}
\alias{CategoricalPop-class}
\alias{CategoricalPop}

\title{Class \code{"CategoricalPop"}}
\description{

  This is a distribution over a fixed set of \dQuote{theta} values which
  assigns a probability to each state.

}
\section{Public Fields}{

  Local to this class.
  \describe{
    \item{\code{$states}}{The set of possible values.  This should have
      the same length as \code{$probs}.}
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
    \item{\code{$pvec}}{The the log of the probabilities.}
    \item{\code{$probs}}{The probabilities for each state.}
  }
}
\section{Methods}{

  \describe{
    \item{\code{$initialize}}{\code{(name,mu,states,probs,tnames,wname)}:
      Constructor, called by \code{$new}.}
    \item{\code{$drawInit}}{\code{(npart,covars=list())}:
      Draws a random starting position. (Used with random quadratures,
      i.e., particle filter, and simulations.)}
    \item{\code{$initProbs}}{\code{signature(theta,covars=list())}:
      Calculates the probability of the initial quadrature.}
    \item{\code{$lprob}}{\code{signature(data,par=self$pvec)}:
      Calculates the log probability of the the starting position.}
    }

}
\section{Constructor}{

The usual expression for building this distribution is
\code{CategoricalPop$new(\var{name},\var{states}=seq(0L,2L,1L),
  \var{probs}=rep(1/length(states),length(states)), tname="theta",
  wname="w")}.

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
apop <- CategoricalPop$new("standard",0:3,rep(1/4,4))
apop$pvec
apop$drawInit(3)
apop$lprob(data.frame(theta=0:3,w=c(1,2,2,1)/6),c(0,0))
apop$initProbs(c(0:3))
}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{ POMDP}
\concept{ HMM}
