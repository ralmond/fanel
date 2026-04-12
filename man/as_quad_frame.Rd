\name{as_quad_frame}
\alias{as_quad_frame}
\alias{as_quad_frame,Quad_Frame-method}
\alias{as_quad_frame,data.frame-method}
\alias{as_quad_frame,matrix-method}
\alias{as_quad_frame,numeric-method}
\title{Converts data into a Quad_Frame}
\description{

  This converts objects into the \code{\linkS4class{Quad_Frame}} form
  for use as a quadrature.  If the argument is not already a Quad_Frame,
  it builds a new Quad_Frame with the data.

}
\usage{
as_quad_frame(x)
\S4method{as_quad_frame}{Quad_Frame}(x)
\S4method{as_quad_frame}{data.frame}(x)
\S4method{as_quad_frame}{matrix}(x)
\S4method{as_quad_frame}{numeric}(x)
}
\arguments{
  \item{x}{A \code{\linkS4class{Quad_Frame}} object or a data frame or
    matrix giving the quadrature points (see details).}
}
\details{

  If the argument is a data frame, it assumes that columns named
  \dQuote{subj}, \dQuote{occ} and \dQuote{quad} are treated as indexes
  for subjects, occasions, and quadrature point, respectively.  All
  other columns are treated as dimensions of the latent variable.  Their
  names are taken from remaining columns.  If \dQuote{subj} is missing
  then \code{bysubj(result)} is set to false; if \dQuote{occ} is
  missing then \code{byocc(result)} is set to false.  If all three are
  missing then the quad index is generated from the row number.

  A matrix is first converted to a data frame; if there are no column
  names, the columns are given names starting with \dQuote{theta_}  A
  numeric vector is converted into a data frame with a single column
  named \dQuote{theta}.

  This function is used in the \code{\linkS4class{Quadrature}$qpoints}
  active field, so data frames and matrixes can be used to set the quad
  points. 


}
\value{

  A object of class \code{\linkS4class{Quad_Frame}}.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{Quad_Frame}}, \code{\linkS4class{Quadrature}}
}
\examples{
  dat <- data.frame(subj=rep(1L:2L,each=12L),
                    occ=rep(rep(1L:4L,2L),each=3L),
                    quad=rep(1L:3L,8L),
                    theta=1:24)
  pd <- quad_frame(dat)
  as_quad_frame(pd)
  pd1 <- as_quad_frame(dat)

  df1 <- data.frame(Theta1=1:3,Theta2=4:6)
  pd2 <- as_quad_frame(df1)

  mat <- matrix(1:6,3,2)
  colnames(mat) <- c("Theta1","Theta2")
  pd3 <- as_quad_frame(mat)

  pd4 <- as_quad_frame(1:3)

}
\keyword{manip}
\keyword{array}
\keyword{chron}
\concept{panel_data}
