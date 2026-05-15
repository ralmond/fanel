\name{simulate.POMDP}
\alias{simulate.POMDP}
\alias{simulate}
\title{Simulate Data from a POMDP Model}
\description{

  This method simulates data from a \code{\linkS4class{POMDP}} model,
  returning a \code{\linkS4class{Panel_Data}} with simulated data and
  latent variables.

}
\usage{
\method{simulate}{POMDP}(object, nsim, seed, ...,
covars = NULL)
}
\arguments{
  \item{object}{A \code{\linkS4class{POMDP}} object which describes the
    model.}
  \item{nsim}{An integer indicating the number of cases to simulate.}
  \item{seed}{An optional seed for the random number generator.}
  \item{\dots}{For other optional arguments.}
  \item{covars}{A \code{\linkS4class{Panel_Data}} class object providing
    covariates.  If supplied, these will be returned with the output
    data.  If not supplied, a new one will be made based on the times in
    the \code{POMDP}.}
}
\details{

  This simulates data using the \code{\link{drawInitial}},
  \code{\link{drawGrowth}}, (used to the generate values of the latent
  variable) and \code{\link{drawData}} (used to generate the data)
  called on the \code{\linkS4class{POMDP}}.  These are added to the
  \code{covars} argument (a new one is made if not supplied).

}
\value{

  The value is a new \code{\linkS4class{Panel_Data}} class which has new
  columns corresponding to \code{dname(object)} and
  \code{qname(object)}, the latter with \dQuote{_sim} appended.

}
\section{Parallel Processing}{

  The work is separated by subject, and each subject is simulated
  independently.  These are split among the workers.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{POMDP}}, \code{\linkS4class{Panel_Data}},
  \code{\link{drawInitial}}
}
\examples{

}
\keyword{ ts }
\concept{ panel_data }
\concept{ simulation }
\concept{ filter }

