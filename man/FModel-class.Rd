\name{FModel-class}
\docType{class}
\alias{FModel-class}
\alias{FModel}
\alias{lprob,FModel-method}
\alias{pvec,FModel-method}
\alias{pvec<-,FModel-method}

\title{Class \code{"FModel"}}
\description{

  This is an abstract class which collects the common properties and
  methods of \code{\linkS4class{PopulationModel}},
  \code{\linkS4class{GrowthModel}} and
  \code{\linkS4class{EvidenceModel}}.  In particular, all of these
  objects have a parameter vector (\code{pvec}) and and log probability
  \code{lprob} function for use with \code{\link{mstep}}.

}
\section{Objects from the Class}{A virtual Class: No objects may be
  created from it.}
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
\section{Extends}{
Class \code{"\link[R6]{R6Class}"}, directly.  Models therefore follow
reference object protocols.
}
\section{Methods}{
  \describe{
    \item{mstep}{\code{(obj = "FModel", data= "data.frame", its="integer",
	control="list", workers = "Workers")}:  Runs the m-step to
      optimize parameters (see section below).}
    \item{\code{$mstep}}{\code{(data, ..., its=3,control=list(),
	workers = NULL)}:  This method does the optimization (see
      section below).} 
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
    \item{\code{$pvec}}{The active property which corresponds to the
      \code{pvec} accessor.}
  }
  Note that all of these delegate to member functions of the objects.
  The default methods generate errors, so these need to be implemented
  for subclasses.

  Additionally, it provides these methods.
  \describe{
    \item{$toString}{Provides a string to identify the model.}
    \item{$print}{Prints the object using the \code{$toString} method.}
  }
}
\author{Russell Almond}
\details{

  The primary purpose of this object is to capture the interface for the
  \code{\link{mstep}}.  The \code{lprob} function takes a set of data,
  which contain the quadrature values of the latent values (in the
  \code{$tnames} columns) in their weights (in the \code{$wname}
  column).

  The \code{pvec} function and its corresponding getters need to get/set
  the parameters on an unbounded domain.  These are passed to
  \code{optim}.  

}
\section{M-step protocol}{

  To support the EM algorithm, subclasses of \code{FModel} need to
  implement the \code{$lprob} and \code{$pvec} methods and possible an
  \code{$mstep} method.  The default method for \code{$mstep} calls
  \code{\link[stats]{optim}} to maximize \code{$lprop(data,par)} as a
  function of \code{par} which has the format of \code{$pvec}.

  The \code{$pvec} method returns the parameters as a vector to be
  passed to used by \code{\link[stats]{optim}}.  This means that
  separate parameters need to be bound into a vector.  They may also
  need to be transformed so that the domain is the whole real line so
  that the optimization is unconstrained.  For example, the parameters
  of the \code{\linkS4class{NormalPop}} are \code{$mu} and
  \code{$sigma}.  The \code{$pvec} method returns \code{c(self$mu,
    log(self$sigma))}.  The setter method for \code{$pvec} must undo
  this transformation.
  
  The \code{$lprobs} method takes two arguments, the \code{data} and
  the parameter (\code{par}).  The latter is in the format determined by
  \code{$pvec}.  The \code{$lprobs} method must decode this to get the
  raw parameters.  It also must extract the columns it needs from the
  \code{data} object (see \code{\link{mstep}()}). It returns a real
  number giving the log of the probability of the data given the
  parameters. 

  The default \code{$mstep} method just runs \code{\link[stats]{optim}}
  to find the maximum log probability.  It returns a list with the name
  of the model and the result from \code{optim}.  It may be the case
  that a faster method is available.  Consider the
  \code{\linkS4class{NormalPop}$mstep} method.  The maximum log probability is
  just the mean and standard devation of the latent variable.  So the
  \code{\linkS4class{NormalPop}$mstep} method overrides the default and
  returns a list containing the name of the model and the parameter values.

}
\seealso{

  \code{\linkS4class{PopulationModel}},
  \code{\linkS4class{GrowthModel}} and
  \code{\linkS4class{EvidenceModel}} are the principle subclasses.
  \code{\linkS4class{ModelSet}} is a set of \code{FModel} objects.

  \code{\link{mstep}} describes the generic mstep algorithm.

}
\examples{
showClass("FModel")
}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{ POMDP}
\concept{ HMM}
