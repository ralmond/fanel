\name{NormalScore}
\Rdversion{1.1}
\docType{class}
\alias{NormalScore-class}
\alias{NormalScore}

\title{Class \code{"NormalScore"}}
\description{

  An evidence model where the observation has a normal error from the
  latent variable.  It was developed for testing, but may be useful for
  "true score" type models.

}
\section{Slots and Active Fields}{

  Local to this class

  \describe{
    \item{$bias}{An offset added to theta to get the mean.}
    \item{$se}{The standard deviation of the error.}
    \item{\code{$pvec}}{The vector \code{c(bias,log(se))}.}
  }

  Inherited fields.

  \describe{
    \item{\code{$convergence}:}{Logical variable indicating whether or
      not the last \code{mstep} converged.}
    \item{\code{$lp}:}{The log-posterior after the last \code{mstep}.}
    \item{\code{$name}:}{A name for the model, primarily used for printing.}
    \item{\code{$wname}:}{The name of the weight column(s). }
    \item{\code{$tnames}:}{The name(s) of the latent variable(s).}
  }
}

\section{Methods}{

  \describe{
    \item{\code{$initialize}}{\code{signature(name,bias,se)}:  The
      constructor, called from \code{$new}.}
    \item{\code{$drawObs}}{\code{signature(theta,covars=list())}:  Draws
      a random response.}
    \item{\code{$llike}}{\code{signature(Y,theta,context=list())}:
      Calculates the log likelihood of the observations given the latent
      variable and current parameters.}
    \item{\code{$lprob}}{\code{signature(data,par=self$pvec)}: Calculated the
      log likelihood of the observations given estimated theta and
      argument parameters.}
    \item{\code{$mstep}}{\code{signature(data,...)}
	Directly optimizes the parameters via mean and std.}
    \item{\code{$print}}{\code{signature(...)}: Prints the object.}
    \item{\code{$toString}}{\code{signature(...)}: Creates a string
      represenation of the object.}

  }
}
\section{Constructor}{

Generally built by calling
\code{NormalScore$new(\var{name},\var{bias},\var{se})}.

}
\section{Superclasses and Subclasses}{

  Its superclass is:
  \code{\linkS4class{EvidenceModel}} which is in turn a subclass of
  \code{\linkS4class{FModel}}.

}
\author{Russell G. Almond}
\seealso{

  \code{\linkS4class{EvidenceModel}}, \code{\linkS4class{FModel}}

}

\examples{
em <- NormalScore$new("ns",1,1/25)
theta <- -2:2
em$drawObs(theta)
em$llike(-1:1,theta)
em$lprob(data.frame(Y=0, theta=theta,w=(1:5)/15),c(0,0))
}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{ POMDP}
\concept{ HMM}
\concept{evidence_model}
