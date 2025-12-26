\name{GrowthModel}
\Rdversion{1.1}
\docType{class}
\alias{GrowthModel-class}
\alias{GrowthModel}

\title{Class \code{"GrowthModel"}}
\description{

  An abstract class representing the distribution of the latent variable
  given its value at the previous occasion.  This class works with the
  \code{\link{simulate.POMDP}} and \code{\link{particleFilter}}
  functions.  The \code{\link{bwFilter}} requires the
  \code{\link{TransitionModel}} class.  (The \code{\link{qFilter}}
  ignores the growth/transition models.)

}
\section{Public and Active Fields}{

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

  The arguments to these functions are as follows:
  \describe{
    \item{theta}{The value of the latent variable at the initial time.}
    \item{deltaT}{The time elapsed between the previous and current
      occasion.} 
    \item{deltaT}{The amount to which the activity was done between the
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

}
\section{Superclasses and Subclasses}{

  Its superclass is \code{\linkS4class{FModel}}.

  Generally, a subclass needs to implment the \code{$pvec} active field
  and the \code{$drawNext}, \code{$lprob}, \code{$toString} and
  \code{$initialize} methods.

  Noted subclass is \code{\linkS4class{TransitionModel}}.

  Existing implementations are:
  \code{\linkS4class{BrownianGrowth}}, \code{\linkS4class{BrownianGrowth2}}

}
\author{Russell G. Almond}
\seealso{

  \code{\linkS4class{POMDP}}, \code{\linkS4class{FModel}}
  \code{\linkS4class{Activities}},
  \code{\linkS4class{BrownianGrowth}},
  \code{\linkS4class{TransitionModel}}

}
\details{

  The biggest difference between growth models and other kinds of
  \code{\linkS4class{FModel}} is the need to account for the time
  between the previous and current occasion.  There are two measures of
  time, \code{deltaT} is the calendar time and it generally affects how
  the latent variable regresses.  The \code{dose} field is how much time
  (or how many times) the subject spends with the activity and affects
  how the activity increases the latent variable.

}
\examples{
MyBrownianGrowth <- R6Class(
  classname="MyBrownianGrowth",
  inherit=GrowthModel,
  public=list(
    initialize = function(name,gain,inovSD,tname="theta",wname="w",
                          dtname="deltaT", dosname="dose") {
      self$name <- name
      self$gain <- gain
      self$inovSD <- inovSD
      self$tnames <- tname
      self$wname <- wname
      self$dtname <- dtname
      self$dosname <- dosname
    },
    gain=0,
    inovSD=.1,
    gain=0,
    inovSD=.1,
    ## Note that the mean depends on dose, and the SD on deltaT
    drawNext = function(theta,deltaT,dose=deltaT,covars=list()) {
      rnorm(length(theta),theta+self$gain*dose,
            self$inovSD*sqrt(deltaT))
    },
    lprob = function(data,par=self$pvec) {
      deltaT <- data[[self$dtname]]
      dose <- data[[self$dosname]]
      if (is.null(dose)) dose <- deltaT
      theta0 <- data[[self$tnames]]
      theta1 <- data[[paste0(self$tnames,"_1")]]
      weights <- data[[self$wname[1]]]
      mu <- theta0 + par[1]*dose
      sig <- exp(par[2])*sqrt(deltaT)
      sum(dnorm(theta1,mu,sig,log=TRUE)*weights)
    },
    mstep = function(data,...) {
      deltaT <- data[[self$dtname]]
      dose <- data[[self$dosname]]
      if (is.null(dose)) dose <- deltaT
      theta0 <- data[[self$tnames]]
      theta1 <- data[[paste0(self$tnames),"_1"]]
      weights <- data[[self$wname[1]]]
      self$gain <- wtd.mean((theta1-theta0)/dose,weights)
      self$inovSD <- wtd.sd((theta1-theta0-self$gain)/sqrt(deltaT),
                             weights)
      list(name=self$name,lprob(data=data))
    },
    toString=function(digits=2,...) {
      paste0("<BrownianGrowth: ", self$name, " ( ",
             round(self$gain,digits=digits),
             ", ",round(self$inovSD,digits=digits)," )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(c(self$gain,log(self$inovSD)))
      self$gain <- value[1]
      self$inovSD <- exp(value[2])
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
