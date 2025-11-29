\name{time2occ}
\alias{time2occ}
\title{Converts time data to occasions}
\description{

  A \dQuote{time} column contains real numbers containing the length of
  time between the beginning of the experiment and the collection of the
  data in this row.  An \dQuote{occ} (occasion) is an integer index of
  the the measurement occasions.  It can either start at 1 (first
  observation) or 0 (a starting time before the first observation).  The
  function \code{time2occ} adds an \dQuote{occ} column to a data frame
  based on the value of the \dQuote{time} column.

}
\usage{
time2occ(data, idcol = "subj", timecol = "time", occcol = "occ", zerobase = FALSE)
}
\arguments{
  \item{data}{The data frame to which the occasion column should be added.}
  \item{idcol}{The name of the column containing the subject identifier.}
  \item{timecol}{The name of the column contaiing the time data.}
  \item{occcol}{The name of the column to be added containing the occasions.}
  \item{zerobase}{A logical value, if true the first occasion is 0; if
    false, 1.}
}
\details{

  This function first splits the data frame by subjects.  Then it orders
  the time values from lowest to highest.  Finally, if
  \code{zerobase==TRUE} it adjusts the first occasions to be zero.

}
\value{
  The \code{data} argumnet with an extra column for \dQuote{occ}
}
\author{Russell Almond}
\seealso{
  \code{\link{long2panel}} which is where it is used.
}
\examples{

longdat <- data.frame(subj=c(rep(1,5),rep(2,3)),time=c(1:5,8:10)*10)
time2occ(longdat)

}
\keyword{ manip }
\keyword{ chron }
\concept{ panel_data }

