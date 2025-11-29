\name{Panel_Frame-class}
\docType{class}
\alias{Panel_Frame-class}
\alias{Panel_Frame}
\alias{[,Panel_Frame-method}
\alias{[<-,Panel_Frame-method}
\alias{nocc,Panel_Frame-method}
\alias{minocc,Panel_Frame-method}
\alias{maxocc,Panel_Frame-method}
\alias{nsubj,Panel_Frame-method}
\alias{isubj,Panel_Frame-method}
\alias{isubj<-,Panel_Frame-method}
\alias{names,Panel_Frame-method}
\alias{get_subj,Panel_Frame-method}
\alias{get_subj<-,Panel_Frame-method}
\alias{as_longform,Panel_Frame-method}
\alias{panel_frame}



\title{Class \code{"Panel_Frame"} -- Data indexed by subject and time.}
\description{

  Panel data is data that are indexed by both a subject, \eqn{i},
  and an occasion, \eqn{j}, so that for a panel data set, \code{x},
  \code{x[\var{i},\var{j}]} refers to a sub-data frame for that subject.
  \code{x[\var{i},\var{j},\var{v}]} refers to variable \eqn{v} in the
  data frame.

}
\section{Objects from the Class}{

  Objects can be created by calls to the function
  \code{panel_frame(\var{data},\var{idcol},\var{occcol},
    \var{zerostart}=TRUE),isubj=NA_integer_},
  where the latter two arguments indicate the columns which indicate the
  subject and occasion indexes.  The function \code{\link{time2occ}} can
  be used to generate occasion indexes from times.

  This function forces the data into cannonical order, sorting by first
  subject (\code{idcol}) and then occasion (\code{occcol}).  It also
  fills in missing combinations of subject and occasion with \code{NA}s.
  If \code{zerostart} is true, then the occasions are numbered starting
  at 0; if false, the minimum occasion is the smallest value in the
  \code{occcol} column.

}
\section{Slots}{
  \describe{
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
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Panel_Frame",i,j,v,...,drop=FALSE)}:
      The \code{i} and \code{j} indexes reference rows of the table
      given the subject(s) and occasion(s).  The \code{v} index is
      optional, and if supplied indicates which column of the data frame
      is returned.  See Details.}
    \item{[<-}{\code{signature(x = "Panel_Frame",i,j,v,...,value)}: sets
      the rows of the table. }
    \item{nocc}{\code{signature(obj = "Panel_Frame")}: Returns the number
      of occasions indexed.  The number of unique occasion indexes.  If
      the occasion index starts at zero, this is one less than the
      highest index.}
    \item{minocc}{\code{signature(obj = "Panel_Frame")}: Returns the smallest
      occaion index, usually 0 or 1.}
    \item{maxocc}{\code{signature(obj = "Panel_Frame")}: Returns the largest
      occaion index.}
    \item{nsubj}{\code{signature(obj = "Panel_Frame")}: Returns the
      number of subjects indexed. }
    \item{isubj}{\code{signature(obj = "Panel_Frame")}: Returns the
      index of the focus subject, or \code{NA} if focused on multiple
      objects.}
    \item{isubj<-}{\code{signature(obj = "Panel_Frame",value = "integer")}:
      Sets the index of the focus subject. }
    \item{names}{\code{signature(x = "Panel_Frame")}:  Returns the names
      of the columns of the underlying data frame.}
    \item{get_subj}{\code{signature(x = "Panel_Frame", subj="integer")}:
      fetches a smaller Panmat focused on subject.  See
      \code{\link{split_subj}}.}
    \item{get_subj<-}{\code{signature(x = "Panel_Frame", subj="integer",
      value="Panmat")}: replaces or inserts smaller Panmat focused
      on subject.}
    \item{as_longform}{\code{signature(x = "Quadrature")}: Converts the
      quadrature into long form (with indexes subj and occ). }
  }
}
\details{

  Panel data can be thought of as a \code{\linkS4class{Panmat}} where
  each element is the row of a data frame.  Like the Panmat, rows and
  columns are automatically replicated if there is only one subject or
  occasion.  In particular, if there is only one subject, then the same
  value will be returned for any value for the subject index.
  Similarly, if there is only one occasion, then all time points will
  return the same value.  The extraction functions, \code{[} and
  \code{[<-} are overriden to support this behavior.

  Data should be sorted with the occasion (\code{j}) index moving fastest
  and the subject (\code{i}) index moving slowest.  Also, the data need to
  be filled with missing data so that the there are the right number of columns.
  Also, the occasion index needs to start with \code{@minocc} field.  The constructor
  function \code{panel_frame()} handles ensuring that the the data are properly organized.

  Note that the extractor returns a data frame, so that individual
  values can be extracted using the \code{$} operator, or using the
  \code{v} argument.

}
\author{Russell Almond}
\note{

  Character indexes are not supported for \code{i} and \code{j}, but are
  useful for \code{v}.
  Negative indexes and logical indexes have not been thoroughly tested.

  Writing to mutliple subjects when the number of subjects is one, or
  writing to multiple occasions when the number of occasions is one is
  likely to have unexpected results.

}
\seealso{
  \code{\link{normalForm}}, \code{\link{as_longform}},
  \code{\linkS4class{Panmat}}

  \code{\link{Evidence}}, \code{\link{Activities}}
}
\examples{

  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value=1:8,value2=101:108)
  pd <- panel_frame(dat)

  pd[1,3]
  pd[1,3]$value2
  pd[,3]$value2
  pd[1,]
  pd[1,3,"value",drop=TRUE]

  pd[2,2]$value <- NA
  pd[2,,"value"]

}
\keyword{classes}
\keyword{array}
\keyword{chron}
\concept{panel_data}
