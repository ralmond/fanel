\name{normalForm}
\alias{normalForm}
\title{Cleans Data for Making Panel_Data Objects}
\description{

  This function does some basic cleaning in preparation for building a
  \code{\linkS4class{Panel_Frame}} object.  It is called by
  \code{\link{long2panel}} and \code{panel_frame}.

}
\usage{
normalForm(data, idcol = "subj", occcol = "occ", zerostart = TRUE)
}
\arguments{
  \item{data}{A data frame in long form.}
  \item{idcol}{A character scalar giving the name of the column which
    contains the subject identifier.}
  \item{occcol}{A character scalar giving the name of the column
    contiaing the occasion identifier.}
  \item{zerostart}{A logical value, if true the indexing starts and zero,
    if false, then one.}
}
\details{

  This function does a bunch of preparation for building a
  \code{\link{Panel_Frame}}.  First it sorts by subject and occasion,
  filling in missing rows with NAs.  Second it calculates the min and
  max occasions as well as the number of subjects.


}
\value{

  A list with the following components
  \item{normdat}{The sorted filled-in data frame.}
  \item{nsubj}{The number of subjects.}
  \item{minocc}{The minimum occasion.}
  \item{maxocc}{The maximum occasion.}

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{Panel_Frame}}, \code{\linkS4class{Panel_Data}},
  \code{\link{long2panel}} and \code{panel_frame}.
}
\examples{
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value=1:8)
  fanel:::normalForm(dat)

  names(dat) <- c("foo","bar","value")
  fanel:::normalForm(dat[,3:1],"foo","bar")

}
\keyword{ internal }
\concept{ Panel_Data }
