\name{Panmat-class}
\docType{class}
\alias{Panmat-class}
\alias{Panmat}
\alias{[,Panmat-method}
\alias{[<-,Panmat-method}
\alias{as.Panmat}
\alias{as.Panmat,Panmat-method}
\alias{as.Panmat,matrix-method}
\alias{as.Panmat,character-method}
\alias{as.Panmat,numeric-method}
\alias{as_longform,Panmat-method}
\alias{nocc,Panmat-method}
\alias{nocc<-,Panmat-method}
\alias{maxocc,Panmat-method}
\alias{maxocc<-,Panmat-method}
\alias{minocc,Panmat-method}
\alias{minocc<-,Panmat-method}
\alias{nsubj,Panmat-method}
\alias{nsubj<-,Panmat-method}
\alias{isubj,Panmat-method}
\alias{isubj<-,Panmat-method}
\alias{panmat}
\alias{mat}
\alias{mat<-}
\alias{diff,Panmat-method}
\alias{cumsum,Panmat-method}
\alias{get_subj,Panmat-method}
\alias{get_subj<-,Panmat-method}


\title{Class \code{"Panmat"} -- A matrix with index replication}
\description{

  A matrix with subject and time indexes.  The panmat will replicate
  over dimensions of size one.  So \code{x[\var{subj},\var{occ}]}
  returns the value of size one.  If the number of subjects (rows) is 1,
  then any value of \code{subj} will return the same value.  Similarly,
  if the number of occasions is 1, then all values for \code{occ} will
  return the same value.


}
\section{Objects from the Class}{

  Objects can be created by calls to \code{panmat(mat, nsubj=nrow(mat),
    nocc=ncol(mat),isubj=NA_integer_}.  Note that if the actual number
  of rows or columns is zero, then \code{nsubj} or \code{nocc} can be
  set to reference the effective number of subjects or occasions
  respectively.

}
\section{Slots}{
  \describe{
    \item{\code{mat}:}{Object of class \code{"matrix"} which is the
      actual matrix. }
    \item{\code{minocc}:}{Object of class \code{"integer"} which contains
      the index of the first occasion, usually 0 or 1.}
    \item{\code{nocc}:}{Object of class \code{"integer"} which contains
      the logical number of occasions (which may be different from the
      number of columns, if the number of columns is 1).}
    \item{\code{nsubj}:}{Object of class \code{"integer"} which contains
      the logical number of subjects (which may be different from the
      number of rows, if the number of rows is 1). }
    \item{\code{isubj}:}{Object of class \code{"integer"} which contains
      the subject on which this object is focused (used when object is
      split using \code{\link{split_subj}}).  Its value is \code{NA} if
      the object covers more than one subject.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Panmat", i, j, ..., drop=FALSE)}:
      Selects elements of the matrix, adjusting indexes in the 1-row or
      1-column cases.  (See details).}
    \item{[<-}{\code{signature(x = "Panmat", i, j, ..., value)}: sets an
      element}
    \item{as.Panmat}{Converts argument into a Panmat if necessary.  If given a vector,
      assumes that the length is the number of occasions. }
    \item{nocc}{\code{signature(obj = "Panmat")}: Returns the number
      of occasions. }
    \item{nocc<-}{\code{signature(obj = "Panmat",value = "integer")}: Sets the
      number of occasions }
    \item{minocc}{\code{signature(obj = "Panmat")}: Returns the minimum index of occasions
      usually 0 or 1. }
    \item{minocc<-}{\code{signature(obj = "Panmat",value = "integer")}: Sets the
      index of the first occasion, usually 0 or 1.}
    \item{maxocc}{\code{signature(obj = "Panmat")}: Returns the maximum occaions index.}
    \item{maxocc<-}{\code{signature(obj = "Panmat",value = "integer")}: Sets the
      maximum occasion index. }
    \item{nsubj}{\code{signature(obj = "Panmat")}: Returns the number of
      subjects.}
    \item{nsubj<-}{\code{signature(obj = "Panmat",value = "integer")}:
      Sets the number of subjects. }
    \item{isubj}{\code{signature(obj = "Panmat")}: Returns the index of the
      focus subject, or \code{NA} if focused on multiple objects.}
    \item{isubj<-}{\code{signature(obj = "Panmat",value = "integer")}:
      Sets the index of the focus subject. }
    \item{mat}{\code{signature(obj = "Panmat")}:  returns the underlying matrix.}
    \item{mat<-}{\code{signature(obj = "Panmat",value="matrix")}:  sets the underlying
      matrix.}
    \item{get_subj}{\code{signature(x = "Panmat", subj="integer")}:
      fetches a smaller Panmat focused on subject.  See
      \code{\link{split_subj}}.}
    \item{get_subj<-}{\code{signature(x = "Panmat", subj="integer",
      value="Panmat")}: replaces or inserts smaller Panmat focused
      on subject.}
    \item{as_longform}{\code{signature(x = "Quadrature")}: Converts the
      quadrature into long form (with indexes subj and occ). }
  }
}
\details{

  The main purpose of this class is to build indexes for groups,
  assessments, activities.  In the case where all subjects have the same
  value, then a 1-row matrix can be used, and \code{x[i,j]} will return
  the same value for all values of \code{i}.  Similarly, if the matrix
  has 1-column, the the value will be the same for all values of
  \code{j}.

}
\author{Russell Almond}
\note{

  Typically longform data is given with times varying faster than subjects,
  but because the rows correspond to subjects, the data may need to be transposed
  or turned into a matrix with `byrow=TRUE`.


  Logical indexes should work, but are not thoroughly tested.  The same
  thing is true for character indexes, although this requires the
  underlying matrix to have row and column names set.  However, the
  testing is light so use with caution.

  Also, it is possible when doing replacements with 1-row and 1-column
  matrixes, that the same element will be replaced multiple times, which
  is likely to produce unexpected results.

}
\seealso{
  \code{\link{as_longform}},
  \code{\linkS4class{Panel_Data}}

  \code{\link{Population}}, \code{\link{Evidence}}, \code{\link{Activities}}

}
\examples{
  pm <- panmat(matrix(1:10,2,5))
  nsubj(pm)
  nocc(pm)
  pm[2,3]
  pm[2,] <- 1:5
  pm[2,3]
  as_longform(pm)

  pm <- panmat(matrix(1:5,1,5))
  pm[2,3]
  pm[1,] <- 6:10
  pm[2,3]
  nsubj(pm) <- 3L
  as_longform(pm)

  pm <- panmat(matrix(1:2,2,1))
  pm[2,3]
  pm[1,] <- 5
  pm[1,3]
  nocc(pm) <- 5L
  as_longform(pm)

}
\keyword{classes}
\keyword{array}
\keyword{chron}
\concept{panel_data}
