\name{Quad_Frame-class}
\docType{class}
\alias{Quad_Frame-class}
\alias{Quad_Frame}
\alias{quad_frame}
\alias{na_quad_frame}
\alias{[,Quad_Frame-method}
\alias{[<-,Quad_Frame-method}
\alias{get_subj,Quad_Frame-method}
\alias{get_subj<-,Quad_Frame-method}
\alias{nquad,Quad_Frame-method}
\alias{nquad<-,Quad_Frame-method}
\alias{qname,Quad_Frame-method}
\alias{qname<-,Quad_Frame-method}

\title{Class \code{"Quad_Frame"} - A \code{"Panel_Frame"} containing
  quadrature data}
\description{

  A Quad Frame contains quadrature points that are indexed by a subject, \eqn{i},
  and an occasion, \eqn{j}, and a quadrature index, \eqn{q} so that for a panel data set, \code{x},
  \code{x[\var{i},\var{j},\var{q}]} refers to a sub-data frame for that
  subject, occasion, quad point, and
  \code{x[\var{i},\var{j},\var{q}, \var{v}]} refers to variable \eqn{v} in the
  data frame.

}
\section{Objects from the Class}{

  Objects can be created by calls to the function 
  \code{quad_frame(\var{data}, \var{idcol}="subj", \var{occcol}="occ",
    \var{qcol}="quad", \var{zerostart}=TRUE), isubj=NA_integer_},
  where the second through fourth arguments indicate the columns which
  indicate the subject, occasion and quadrature indexes.  The function
  \code{\link{time2occ}} can  be used to generate occasion indexes from times.

  This function forces the data into cannonical order, sorting by first
  subject (\code{idcol}) and then occasion (\code{occcol}) and finally
  by quadrature.  It also fills in missing combinations of subject and
  occasion with \code{NA}s.  If \code{zerostart} is true, then the
  occasions are numbered starting at 0; if false, the minimum occasion
  is the smallest value in the \code{occcol} column.

  A Quad_Frame filled with NAs can be created by using the function
  \code{na_quad_frame(\var{nsubj}, \var{minocc}, \var{maxocc},
  \var{nquad}, \var{qnames}="theta", \var{isubj}=NA_integer_)}.  This is
  used for \code{\linkS4class{ParticleQuad}}s.



}
\section{Slots}{
  \describe{
    \item{\code{nquad}:}{Object of class \code{"integer"} indicating the
      number of quadrature points.}
  }
  Slots inherited from \code{\linkS4class{Panel_Frame}}.
    \describe{
    \item{\code{byocc}:}{A logical field, if true the data are different
      for each occasion.}
    \item{\code{bysubj}:}{A logical field, if true the data are different
      for each subject.}
    \item{\code{dat}:}{Object of class \code{"data.frame"} in canonical
      order (see details).}
    \item{\code{nsubj}:}{Object of class \code{"integer"} describing the
      number of subjects}
    \item{\code{minocc}:}{Object of class \code{"integer"} giving the
      minimum index.  This is mainly useful for distinguishing between
      time indexes starting at 0 and 1.}
    \item{\code{nocc1}:}{Object of class \code{"integer"} number of
      occasions minus 1.}
    \item{\code{isubj}:}{Object of class \code{"integer"} which contains
      the subject on which this object is focused (used when object is
      split using \code{\link{split_subj}}).  Its value is \code{NA} if
      the object covers more than one subject.}
  }

  
}
\section{Extends}{
Class \code{"\linkS4class{Panel_Frame}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{x = "Quad_Frame",i,j,q,v,...,drop=FALSE]}:  The
      \code{i}, \code{j}, \code{q} indexes reference rows of the table 
      given the subject(s), occasion(s), and quadrature point(s).  The
      \code{v} index is optional, and if supplied indicates which column
      of the quadrature is returned.  See Details.}
    \item{[<-}{\code{x = "Quad_Frame"),i,j,q,v,...]<-value}: sets the
      quadrature points. }
    \item{as_quad_frame}{\code{(x = "Quad_Frame")}: Coerces argument
      into a Quad_Frame.}
    \item{get_subj}{\code{(x = "Quad_Frame", isub="integer")}:
      fetches a smaller Quad_Frame focused on subject.  See
      \code{\link{split_subj}}.}
    \item{get_subj<-}{\code{(x = "Quad_Frame", isub="integer") <-
      value="Quad_Frame"}: replaces or inserts smaller Quad_Frame focused
      on subject.}
    \item{nquad}{\code{signature(obj = "Quad_Frame")}: Returns/sets the
      number of quadrature points }
    \item{qname}{\code{signature(obj = "Quad_Frame")}: Returns/sets the
      names of the quadrature varaibles. }
  }

  Methods inherited from \code{\linkS4class{Panel_Frame}}:
  \describe{
    \item{byocc}{\code{(obj = "Panel_Frame")}: Returns (sets) the by
      occasion flag.}
    \item{bysubj}{\code{(obj = "Panel_Frame")}: Returns (sets) the by
      subject flag.}
    \item{nocc}{\code{(obj = "Panel_Frame")}: Returns (sets) the number
      of occasions indexed.  The number of unique occasion indexes.  If
      the occasion index starts at zero, this is one less than the
      highest index.}
    \item{minocc}{\code{(obj = "Panel_Frame")}: Returns (sets) the smallest
      occaion index, usually 0 or 1.}
    \item{maxocc}{\code{(obj = "Panel_Frame")}: Returns (sets) the largest
      occaion index.}
    \item{nsubj}{\code{(obj = "Panel_Frame")}: Returns (sets) the
      number of subjects indexed. }
    \item{isubj}{\code{(obj = "Panel_Frame")}: Returns (sets) the
      index of the focus subject, or \code{NA} if focused on multiple
      objects.}
    \item{as_longform}{\code{(x = "Quad_Frame")}: Converts the
      Quad_Frame into long form (with indexes subj, occ, and quad). }
  }

}
\details{

  A quad frame can be thought of as a \code{\linkS4class{Panmat}} where
  each element is the row of a data frame giving the quadrature points.
  It differs from its parent \code{\linkS4class{Panel_Frame}} in that it
  takes a third index for quadrature points.  Like the Panmat, rows and
  columns are automatically replicated if \code{@bysubj} or
  \code{@byocc} is false.  In particular, if \code{@bysubj} is false,
  then the same value will be returned for any value for the subject
  index.  Similarly, if \code{@byocc} is false, then all time points
  will return the same value.  The extraction functions, \code{[} and
  \code{[<-} are overriden to support this behavior.

  Data should be sorted with the quadrature point index (\code{q})
  moving fastest, the occasion (\code{j}) index moving second fastest
  and the subject (\code{i}) index moving slowest.  Also, the data need
  to be filled with missing data so that the there are the right number
  of columns.  Also, the occasion index needs to start with
  \code{@minocc} field.  The constructor function \code{quad_frame()}
  handles ensuring that the the data are properly organized.

  Note that the extractor returns a data frame, so that individual
  values can be extracted using the \code{$} operator, or using the
  \code{v} argument.

}
\author{Russell Almond}
\note{

  Character indexes are not supported for \code{i}, \code{j}, and
  \code{q}, but are useful for \code{v}.  Negative indexes and logical
  indexes have not been thoroughly tested.

  Writing to mutliple subjects when the number of subjects is one, or
  writing to multiple occasions when the number of occasions is one is
  likely to have unexpected results.

}
\seealso{

  \code{\link{normalForm}}, \code{\link{as_longform}},
  \code{\linkS4class{Panel_Frame}}, \code{\link{as_quad_frame}}

  \code{\linkS4class{Quadrature}}



}
\examples{

  df <- data.frame(quad=1:3,theta=c(-1,0,1))
  qf <- quad_frame(df)
  nquad(qf)  # 3L
  bysubj(qf) # False
  byocc(qf) # False
  qf[1,1,]  # c(-1,0,1)
  qf[2,2,2]$theta # 0

}
\keyword{classes}
