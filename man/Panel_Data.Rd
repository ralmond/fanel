\name{Panel_Data-class}
\docType{class}
\alias{Panel_Data-class}
\alias{nocc,Panel_Data-method}
\alias{minocc,Panel_Data-method}
\alias{maxocc,Panel_Data-method}
\alias{nsubj,Panel_Data-method}
\alias{nsubj<-,Panel_Data-method}
\alias{panel_data}
\title{Class \code{"Panel_Data"} --  Time and DeltaT}
\description{

  The objects looks at panel data, which is indexed by both subject and
  occasion.  It consists of three parts:  the time (\code{$time}) and
  time differnce (\code{$dt}), time variant data (\code{$vari}), and
  time invariant data, (\code{$invar}).  The
  \code{$getVar(\var{subj},\var{occ})} gets the data relevant to a
  particular subject at a particular time.

}
\section{Active Fields}{

  \describe{
    \item{time}{A \code{\linkS4class{Panmat}} mapping measurement occasions to
      times.}
    \item{dt}{A \code{\linkS4class{Panmat}} mapping measurement occasions to
      the difference in time between the reference occasion and the
      previous one.}
    \item{vari}{A \code{\linkS4class{Panel_Frame}} object giving time
      dependent data.}
    \item{invar}{A \code{\link{data.frame}} giving time invariant
      data.}
    \item{datacols}{A character vector giving the names of the
      \code{$vari} object which are considered data.}
  }
}
\details{

  Panel data is data about one or more subjects measured on more than
  one occasion, not necessarily regularly spaced in time.  The
  \code{$time} field gives the time of each occasion (it can be the same
  or different across subjects); the \code{$time} array occasion index
  starts at 0.  The \code{$dt} field gives the differnce in time between
  the referenced occasion and the previous one; its occasion indexe
  starts at 1.

  While the \code{$time} or \code{$dt} fields are required (one is
  automatically set from the other), the additional data fields are
  optional.  There are two kinds of data:  time variant data
  (\code{$vari}), which is indexed by both subject and occasion, and
  time invariant data (\code{$invar}), which is only indexed by subject.

  The public method \code{$getVar(\var{subj},\var{occ})} joins the rows
  of the \code{$vari} data for the referenced subject and occasion, and
  the rows of the \code{$invar} data for the referenced subject as a
  data frame (so variables can be accessed by column name).

  The response data (i.e., the data which the
  \code{\link{EvidenceModel}} provides a likelihood for) is variables (the
  \code{v} index) of the \code{$vari} data.  The value of the
  \code{$datacols} field is the names of those variables.  If they are
  not in variant data, new variables are created and set to \code{NA}

}
\section{Public Methods}{

  \describe{
    \item{initialize}{\code{signature(time = "Panmat", dt = "Panmat",
	vari = "Panel_Frame", invar = "data.frame",
	datacols=character())}:  Generator.
      Exactly one of \code{time} and \code{dt} should be supplied.  The
      other arguments are optional}
    \item{print}{\code{signature(x, ...)}:  Prints the object}
    \item{getVar}{\code{signature(subj = "integer", occ = "integer")}:
      Returns a data frame containing relevant variant and invariate
      variables.}
    \item{getData}{\code{signature(subj = "integer", occ = "integer")}:
      gets the data variables for the indexed subject and occasion.}
    \item{setData}{\code{signature(subj = "integer", occ = "integer",
	value)}: sets the data variables for the indexed subject and
      occasion.}
  }
}

\section{S4 Methods}{
  \describe{
    \item{as_longform}{\code{signature(x = "Panel_Data")}: Converts to a
      longform tibble.}
    \item{nocc}{\code{signature(obj = "Panel_Data")}: returns the number
      of occasions.)}
    \item{minocc}{\code{signature(obj = "Panel_Data")}: returns the index of the
      minimum occasion, usually 0 or 1.)}
    \item{maxocc}{\code{signature(obj = "Panel_Data")}: returns the index of th
      maximum occasions.)}
    \item{nsubj}{\code{signature(obj = "Panel_Data")}: returns the
      number of subjects. }
    \item{nsubj<-}{\code{signature(obj = "Panel_Data")}: sets the number
      of subjects (for one subject data.)}
  }
}
\section{Constructor}{

  There are two constructors (in addition to the \code{new} method).
  The first is the \code{panel_data(\var{time},\var{dt},\var{vari},
    \var{invar}, \var{datacols})} method.
  Only one of \code{time} \code{dt} should be
  supplied and it should be a matrix giving the times or time
  differences.  If \code{vari} is supplied, it should be an
  \code{\linkS4class{Panel_Frame}} or a data frame with \dQuote{subj}
  and \dQuote{occ} columns.  If \code{invar} if supplied should be a
  data frame with as many rows as there are subjects.  The
  \code{datacols} should be the names of

  Alternatively, the function \code{\link{long2panel}} will convert a
  data frame in long form (with occasion and subject indexes) as well as
  a time column into a \code{Panel_Frame}.

}
\section{Superclasses and Subclasses}{

  This is an instance of \code{\link[R6]{R6Class}}.  The \code{$time}
  and \code{$dt} fields are objects of type \code{\linkS4class{Panmat}}.
  The \code{$vari} field is an object of type
  \code{\linkS4class{Panel_Frame}}.

}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{Panel_Frame}}, \code{\linkS4class{Panmat}},
  \code{\link{long2panel}}, \code{\link{as_longform}}
}
\examples{
showClass("Panel_Data")
}
\keyword{classes}
\keyword{array}
\concept{panel_data}
