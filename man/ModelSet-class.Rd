\name{ModelSet-class}
\docType{class}
\alias{ModelSet-class}
\alias{ModelSet}
\alias{maxocc,ModelSet-method}
\alias{maxocc<-,ModelSet-method}
\alias{minocc,ModelSet-method}
\alias{minocc<-,ModelSet-method}
\alias{mstep,ModelSet-method}
\alias{nocc,ModelSet-method}
\alias{nocc<-,ModelSet-method}
\alias{nsubj,ModelSet-method}
\alias{nsubj<-,ModelSet-method}

\title{Class \code{"ModelSet"}}
\description{

  An abstract container class for models.

}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots and active fields}{
  \describe{
    \item{\code{name}:}{A \code{"character"} scalar giving the
      a name for the collection. }
    \item{\code{models}:}{A \code{"list"} of 
      \code{\linkS4class{FModel}} objects containing the actual models.}
    \item{\code{index}:}{Object of class \code{"Panmat"} giving the
      indexes of which model is used by which (subject, occasion)
      combination.}
    \item{\code{tnames}:}{A \code{"character"} vector giving the names
      of the latent variables. }
    \item{\code{wname}:}{A \code{"character"} vector giving the name(s) of
      the weight columnn. }
    \item{\code{dnames}:}{A \code{"character"} vector giving the name(s)
      of the data column.}
  }
}
\section{Extends}{
  Class \code{"\link[R6]{R6Class}"}, directly.

  The subclasses are:  \code{\linkS4class{Population}},
  \code{\linkS4class{Activities}}, \code{\linkS4class{ActivitiesD}} and
  \code{\linkS4class{Evidence}}. 
  
}
\section{S4 Methods and class methods }{
  \describe{
    \item{\code{maxocc},\code{maxocc<-}}{:  Gets/sets
      the maximum  occasion associated with the index data. }
    \item{\code{minocc}, \code{minocc<-}}{: Gets/sets the minimum
      occasion associated with the index data.  Usually 0 or 1. }
    \item{\code{mstep}}{Implements the mstep (see below).}
    \item{\code{nocc},\code{nocc<-}}{Gets/sets the number of occasions.}
    \item{\code{nsubj},\code{nsubj<-}}{Gets/sets the number of subjects.}
  }

  Some additional object methods 

  \describe{
    \item{\code{$toString}}{Generates a charcter representation.}
    \item{\code{$print}}{Prints the object; default method uses
      \code{$toString} so only that needs to be modified.}
    \item{\code{$mstep}}{\code{signature(data = "Panel_Data",
	its = "integer", control = "list", workers = "Workers")}:
      This is the implementation of the m-step.  This method must be
      implemented by the sub-classes.}
  }

}
\details{

  The key idea of this class is that it is a collection of models.  The
  index describes which model should be used.

  The m-step method implementations generally split the data according
  to models being used, and then delegates to the models to do the
  calculation. 

}
\author{Russell Almond}
\seealso{
  The subclasses are:  \code{\linkS4class{Population}},
  \code{\linkS4class{Activities}}, \code{\linkS4class{ActivitiesD}} and
  \code{\linkS4class{Evidence}}. 

  \code{\linkS4class{FModel}}

}
\examples{
showClass("ModelSet")
}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
