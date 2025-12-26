\name{as_longform}
\alias{as_longform}
\alias{longform}
\title{Converts panel data to a longform data frame.}
\description{

  The \code{as_longform} methods unwind fanel objects (objects of class
  \code{\linkS4class{Panmat}}, \code{\linkS4class{Panel_Frame}},
  \code{\linkS4class{Panel_Data}}, \code{\linkS4class{Quadrature}},
  \code{\linkS4class{ModelSet}}, and \code{\linkS4class{POMDP}}) into a
  data frame in long form data frame (or tibble) with rows corresponding
  to subjects and occasions. 

  The \code{longform} function combines the quadrature, model and data
  elements in the same data frame.

}
\usage{
as_longform(x, ...,n = nsubj(x), maxocc = maxocc(x), minocc = minocc(x),
            weightType="all",
            name = deparse(substitute(x)))
longform(quad,model,data)
}
\arguments{
  \item{x}{The data object to be converted into long form data.}
  \item{n}{The number of subjects.}
  \item{maxocc}{The maximum occurance value.}
  \item{minocc}{The minimum occurance value, usually 1 or 0.}
  \item{weightType}{The types of weights to get (only used with
    \code{\linkS4class{Quadrature}} classes).}
  \item{name}{The name to give the column for a \code{Panmat}.}
  \item{\dots}{Separates the required and optional arguments.}
  \item{quad}{A \code{\linkS4class{Quadrature}}.}
  \item{model}{A \code{\linkS4class{POMDP}} and
    \code{\linkS4class{ModelSet}} class.}
  \item{data}{A \code{\linkS4class{Panel_Data}} object.}
}
\details{

  The panel data has indexes for both subject and occassion, this turns
  the data into a long form tibble with with two index columns,
  \dQuote{subj} and \dQuote{occ}.  These are suitable for use with
  \code{\link[dplyr]{left_join}} to build up long form data.

  In the case of \code{\linkS4class{Quadrature}} classes, there is an
  additional index \dQuote{quad} corresponding to quadrature points.
  The functions \code{\link{summaryq}} and \code{summaryqq} reduce over
  the needed dimensions.

}
\value{
  A \code{\link[tibble]{tibble}} with columns \dQuote{subj},
  \dQuote{occ} and possibly \dQuote{quad} giving the subject and
  occasion index.
}
\author{Russell Almond}
\seealso{
  Objects with methods for this generic function.
  \code{\linkS4class{Panmat}}, \code{\linkS4class{Panel_Frame}},
  \code{\linkS4class{Quadrature}},
}
\examples{

}
\keyword{manip}
\keyword{array}
\keyword{chron}
\concept{panel_data}

