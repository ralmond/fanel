\name{ModelSet-class}
\docType{class}
\alias{ModelSet-class}
\alias{ModelSet}
\alias{maxocc.ModelSet}
\alias{maxocc<-.ModelSet}
\alias{minocc.ModelSet}
\alias{minocc<-.ModelSet}
\alias{mstep.ModelSet}
\alias{nocc.ModelSet}
\alias{nocc<-.ModelSet}
\alias{nsubj.ModelSet}
\alias{nsubj<-.ModelSet}
\alias{nmodels.ModelSet}

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
    \item{\code{iname}:}{String giving the name of the index (e.g.,
      \dQuote{group}, \dQuote{task}, or \dQuote{action}).}
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
    \item{\code{nsubj},\code{nsubj<-}}{Gets/sets the number of
      subjects.}
    \item{\code{nmodels}}{Returns the number of models.}
    \item{as_longform}{\code{signature(x = "ModelSet")}: Returns
      longform (\code{link{as_longform}}) data consisting of the index
      variable. }
  }

  Some additional object methods 

  \describe{
    \item{\code{$toString}}{Generates a charcter representation.}
    \item{\code{$print}}{Prints the object; default method uses
      \code{$toString} so only that needs to be modified.}
    \item{\code{$prepData}}{Prepares data for the mstep calculations.
      Defaults to identity function, but is often overridden by
      subclasses.} 
    \item{\code{$split_m}}{Assigns data to models and builds a list of
      (model, data) pairs.}
    \item{\code{$mstep}}{\code{signature(data = "Panel_Data",...,
	its = "integer", control = "list", workers = "Workers")}:
      This is the implementation of the m-step.  Generic method calls
      \code{$prepData} and \code{$split_m} and then runs over the pairs.}
  }

}
\details{

  The key idea of this class is that it is a collection of models.  The
  index describes which model should be used.

  The m-step method implementations generally split the data according
  to models being used, and then delegates to the models to do the
  calculation. 

}
\section{M-step}{

  The data set passed into the \code{ModelSet$mstep(data)} method is the
  complete data set over all subjects and occasions (see
  \code{\link{mstep}()} for the complete list of columns).  The primary
  job of the model set methods is to assign the data rows to the models
  which are appropriate.  The \code{$split_m} method does this work.

  First, \code{$split_m} call the \code{$prepData(data)} method which
  does any preparation necessary.  The default is the identity function.
  The \code{\linkS4class{Population}} class filters the data to include
  just Occasion 0.  The \code{\linkS4class{Activities}} class adds the
  \dQuote{theta_1} columns.  This may need to be overridden for
  new subclasses of \code{ModelSet}.  

  Next, \code{$split_m} splits the data according to the \code{$iname}
  column (by default \dQuote{group}, \dQuote{task}, or \dQuote{action}).
  It returns a list of \code{(model,data)} pairs where \code{model} is
  one of the models, and \code{data} are the rows of the data argument
  which are associated with that model.

  The \code{$mstep} method takes the \code{(model, data)} pairs and
  apply \code{model$mstep(data)} method.  It uses the \code{workers}
  argument to divide the work among threads on the computer.  In order
  to allow the workers to be shared among the three model sets, the
  \code{mstep(\linkS4class{POMDP})} method calls \code{$split_m} on the
  models and builds a master list of \code{model, data)} pairs and then
  assigns them to the workers.
  
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
