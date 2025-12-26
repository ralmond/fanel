\name{getTime}
\alias{getTime}
\alias{getTime<-}
\alias{getDT}
\alias{getDT<-}


\title{Accesses Time Tables for Fanel Objects}
\description{

  Accesses the observation times (\code{getTime}) or differences
  between observation times (\code{getDT}) for
  \code{\linkS4class{Panel_Data}}, \code{\linkS4class{Quadrature}}s,
  \code{\linkS4class{ModelSet}} and \code{\linkS4class{POMDP}} object.  

}
\usage{
getTime(obj)
getTime(obj) <- value
getDT(obj)
getDT(obj) <- value

}
\arguments{
  \item{obj}{A \code{\link{Panel_Data}},
    \code{\linkS4class{Quadrature}}, \code{\linkS4class{ModelSet}}
    or \code{\linkS4class{POMDP}} object.}
  \item{value}{A \code{\linkS4class{Panmat}} giving the new times.} 
}
\value{

  A \code{\linkS4class{Panmat}} mapping subject--occasions to times or
  differences in times.  

}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{Panmat}}, \code{\linkS4class{Panel_Data}}
  \code{\linkS4class{Quadrature}}, \code{\linkS4class{ModelSet}},
  \code{\linkS4class{POMDP}}
}
\examples{
}
\keyword{ attribute }
\concept{ panel_data }

