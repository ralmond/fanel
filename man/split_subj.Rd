\name{split_subj}
\alias{split_subj}
\alias{get_subj}
\alias{get_subj<-}
\alias{add_subj}
\alias{bind_subj}
\title{Tools for splitting and separating fanel objects.}
\description{

  These are tools for splitting and rejoining \code{fanel} data
  objects, including \code{\linkS4class{Panmat}},
  \code{\linkS4class{Panel_Frame}},
  \code{\linkS4class{Panel_Data}} and \code{\linkS4class{Quadrature}}
  objects by subject.

}
\usage{
split_subj(x)
get_subj(x,isubj)
get_subj(x,isubj) <- value
add_subj(x,xnew)
bind_subj(xlist)
}
\arguments{
  \item{x}{An object with one of the fanel data classes.}
  \item{isubj}{An (integer) index for a particular subject.}
  \item{xnew, value}{A new object of the same type as \code{x}
    representing the data to be added.}
  \item{xlist}{A list of fanel data objects all of the same type.}
}
\details{

  These tools basically exist to allow splitting the data into a list of
  smaller objects which can then be processed in parallel by
  \code{\link{Workers}} (or other parallel processing solutions).  The
  function \code{split_subj} turns a data object into a list of objects
  each focused on a single subject.  The function \code{bind_subj} glues
  them back together again.

  The functions \code{get_subj} and \code{get_subj<-} do much of the
  work.  These are in fact generic functions and must be specified for
  each class which supports this protocol.  The expression
  \code{xnew <- get_subj(x,isubj)} extracts a subject-focused object with
  \code{\link{isubj}(xnew)==isubj}.  The expression
  \code{get_subj(x,subj)<-xnew} adds or replaces the data for the
  subject.  The function \code{add_subj(x,xnew)} is a synonym
  for \code{get_subj<-} using either the the subject number in
  \code{\link{isubj}(xnew)} or \code{\link{nsubj}(x)+1} if the former is
  \code{NA}.

}
\value{
  The function \code{split_subj} returns a list of objects of the same
  type as its argument.  The function \code{bind_subj} returns an object
  of the same type as the elements of its argument.

  The other functions return the modified first argument.

}
\author{Russell Almond}
\note{

  The objects that are currently supported by this protocol are
  \code{\linkS4class{Panmat}}, \code{\linkS4class{Panel_Frame}},
  \code{\linkS4class{Panel_Data}} and \code{\linkS4class{Quadrature}}.

  To make other object types work with \code{split_subj}, \code{add_subj},
  and \code{bind_subj}, they must implement methods for \code{get_subj},
  \code{get_subj<-}, \code{\link{isubj}} and \code{\link{nsubj}}.

}
\seealso{

  Implementing classes:
  \code{\linkS4class{Panmat}}, \code{\linkS4class{Panel_Frame}},
  \code{\linkS4class{Panel_Data}} and \code{\linkS4class{Quadrature}}.

  Utility functions:
  \code{\link{isubj}} and \code{\link{nsubj}}.

  The \code{\link{Workers}} class provides a mechanism for splitting the
  workload according to a split.

}
\examples{
\dontrun{
  workers <- Workers$new()
  workers.start()
  workers.lapply(split_subj(bigdat) \(dat) {
    ##...
  }) |> bind_subj() -> result
}
}
\keyword{ manip }
\concept{ panel_data }

