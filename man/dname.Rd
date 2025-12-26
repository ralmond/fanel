\name{dname}
\alias{dname}
\alias{dname<-}
\alias{tname}
\alias{tname<-}
\alias{wname}
\alias{wname<-}

\title{Accesses column names for special data columns.}
\description{

  Accesses the column names of the data (\code{dname}), latent variable 
  (\code{tname}), or weight (\code{wname}) columns, for an
  \code{\linkS4class{FModel}}, \code{\linkS4class{ModelSet}}, or
  \code{\linkS4class{POMDP}} object.

}
\usage{
dname(obj)
dname(obj) <- value
tname(obj)
tname(obj) <- value
wname(obj)
wname(obj) <- value
}
\arguments{
  \item{obj}{A \code{\linkS4class{FModel}},
    \code{\linkS4class{ModelSet}} or 
    \code{\link{POMDP}} object.}
  \item{value}{A character vector giving the names of the relevant
    columns.}
}
\value{
  An character vector giving the names of the relevant columns.
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{FModel}}, \code{\linkS4class{ModelSet}}
    \code{\link{POMDP}}
}
\examples{
}
\keyword{ attribute }
\concept{ panel_data }

