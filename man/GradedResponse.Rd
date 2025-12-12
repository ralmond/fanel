\name{GradedResponse}
\Rdversion{1.1}
\docType{class}
\alias{GradedResponse-class}
\alias{GradedResponse}

\title{Class \code{"GradedResponse"}}
\description{

  An implementation of Samejimia's (1969) graded response model.  Two
  parameters, \code{a}, a discrimination and \code{b} a vector of
  difficulties, where
  \deqn{Pr(Y\ge k|\theta, a, b) = \mathrm{logit}^{-1}(a\theta -b[k])}.
  So the possible values for Y are integers from 0 to \code{length(b)}.

}
\section{Slots and Active Fields}{

  Local to this class.

  \describe{
    \item{\code{$a}}{A discrimination parameter, should be positive.}
    \item{\code{$b}}{A vector of difficulties, should be increasing.}
    \item{\code{$pvec}}{The parameter vector is \code{c(log(a),b)}.}
  }

  Inherited Fields:
  
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
    \item{\code{$cuts}}{\code{signature(theta,a=self$a,b=self$b)}:
      Generates the cut points (cumulative probabilities).}
    \item{\code{$initialize}}{\code{signature(name,a,b)} called from the
      \code{$new} method.} 
    \item{\code{$drawObs}}{\code{signature(theta,covars=list())}:  Draws
      a random response.}
    \item{\code{$llike}}{\code{signature(Y,theta,context=list())}:
      Calculates the log likelihood of the observations given the latent
      variable and current parameters.}
    \item{\code{$lprob}}{\code{signature(data,par=self$pvec)}: Calculated the
      log likelihood of the observations given estimated theta and
      argument parameters.} 
    \item{\code{$mstep}}{\code{signature(data,...,
	its=3,control=list())}:  Runs the optimizer for \code{its} steps.}
  }

  Method parameters:
  \describe{
    \item{name}{Identifier for model.}
    \item{theta}{Parameter value for evaluation}
    \item{a}{Discrimination, should be a positive scalar.}
    \item{b}{A vector of difficulties, should be increasing.}
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
\section{Constructor}{

The recommended way to build this model is
\code{GradedResponse$new(\var{name},\var{a},\var{b})}.

}
\section{Superclasses and Subclasses}{

  Instance of \code{\linkS4class{EvidenceModel}} which is in turn
  \code{\linkS4class{FModel}}.

}
\author{Russell G. Almond}
\seealso{

  \code{\linkS4class{EvidenceModel}},   \code{\linkS4class{FModel}}.

}
\references{

  Samejima, F. (1969). Estimation of latent ability using a response
  pattern of graded scores. _Psychometrika Monograph No. 17_, 34(4), (Part
  2).
  \url{https://www.psychometricsociety.org/sites/main/files/file-attachments/mn17.pdf}

}
\examples{

gr <- GradedResponse$new("agr",1,c(-1,1))
gr
gr$pvec
gr$drawObs(c(-1,0,1))
gr$llike(c(0,1,0),c(-1,0,1))
dat <- data.frame(Y=c(0,1,0),theta=c(-1,0,1),w=c(.4,.3,.3))
gr$lprob(dat,gr$pvec)

}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{ POMDP}
\concept{ HMM}
\concept{evidence_model}
