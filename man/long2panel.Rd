\name{long2panel}
\alias{long2panel}
\title{Build Panel_Data from a long form data frame.}
\description{

  This takes data in long form, with indexes for subject (\code{idcol}),
  time (\code{timecol}) and occaions (\code{occcol}), and builds a
  \code{\linkS4class{Panel_Data}} which contains the information.

}
\usage{
long2panel(df, idcol = "subj", timecol = "time", occcol = "occ",
           datacols = character(), invcols = character(), invar = NULL)
}
\arguments{
  \item{df}{A data frame with columns identifying subject (\code{idcol}), time (\code{timecol})
    and occasions \code{occcol}.}
  \item{idcol}{The name of the column containing the subject identifier
    (default is \dQuote{subj}).}
  \item{timecol}{The name of the column giving the observation times, default \dQuote{time}.}
  \item{occcol}{The name of the column giving occasion identifiers, default \dQuote{occ}.
    If this column is not present, it is built from the time column.}
  \item{datacols}{The names of the columns which contain the data (whose distribution
    is modeled by the evidence models).  If these columns are not present, then new columns
    are created and filled with \code{NA}s.}
  \item{invcols}{The names of columns which contain time invariant data.
    Only values at 0 occasions are used.}
  \item{invar}{A supplementary data frame with rows corresponding to the subjects for
    time invariant data.}
}
\details{

  Panel data consist of measurements made over multiple subjects,
  on multiple occasions, not necessarily evenly spaced in time.
  The occasion index starts at zero, which is often a preliminary state before data are
  collected, but serves as a starting point for the time series. While most data
  values value by time (variant data) some do not (invariant data).  Sometimes invariant
  data are stored in a separate data frame and sometimes as data values at time point zero.

  The most convenient way to store data for such models is long form data
  (see \code{dplyr::\link[tidyr]{pivot_longer}}).  There are two index columns,
  nominally, \dQuote{subj} (subject index) and \dQuote{occ} (occasion index),
  although the \code{idcol} and \code{occcol} fields can be used to indicate a
  different name.  If there is no occasion column, but there is a time column, then the
  occasion index can be built.  The \code{timecol} field indicates which column the time
  codes are in.  This field must be supplied.

  There are two kinds of data columns:  data and covariates.  Data are the observations
  whose probability distributions are given by the \code{\linkS4class{Evidence}}.  These
  are the ones returned by the \code{\linkS4class{Panel_Data}$getData()} function.
  The other are covariates; both data and covariates are returned by the
  \code{\linkS4class{Panel_Data}$getVar()} method.  The \code{datacols} argument gives
  the names of the data data columns.  Note that if these columns do not exist, they are
  created and filled with \code{NA}s, which is useful for use with \code{\link{simulate.POMDP}}.

  Time invariant data can be entered in two ways:  first it can be placed in occasion zero,
  and marked with the \code{invcols} argument.  Second, it can be placed in a separate data
  frame passed through the \code{invar} argument which should be data frame with rows
  corresponding to the subjects.

  The function also fills in the missing values.


}
\value{

  A \code{\linkS4class{Panel_Data}} object containing the data.

}
\author{Russell Almond}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
\keyword{ array }
\keyword{ classes }
\concept{ panel_date }
