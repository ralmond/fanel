\name{Activities-class}
\docType{class}
\alias{Activities-class}
\alias{Activities}
\alias{ActivitiesD-class}
\alias{ActivitiesD}
\alias{advanceWeights.ActivitiesD}
\alias{retreatWeights.ActivitiesD}
\alias{nsubj<-.Activities}
\alias{nocc<-.Activities}
\alias{maxocc<-.Activities}
\alias{minocc<-.Activities}
\alias{drawGrowth.Activities}
\alias{as_longform.Activities}

\title{Class \code{"Activities"}}
\description{

  A container for a collection of \code{\linkS4class{GrowthModel}}s.
  Note that unlike the other subclasses of
  \code{\linkS4class{ModelSet}}, \code{Activities} has fields
  \code{$dt} which holds the elapsed time between occasions, and
  \code{$dose} wihich describes the time spent on the activity between
  occaions.

  \code{ActivitiesD} is a subclass of \code{Activities} which is
  designed to work with the \code{\link{BWFilter}}.  It particular,
  \code{ActivitiesD} supports the \code{$rmat()}, \code{$tmat()},
  \code{$advance()} and \code{$retreat()} methods needed for that filter.

  
}
\section{Constructor}{

  The constructor is
  \code{Activities$new(name, growthModels=list(),
    actions=1L, dt=1.0, dosage=NULL, tname="theta",
    wname="w", dtname="deltaT", dosname="dose")} or
  \code{ActivitiesD$new(...)}. 


  The arguments for the constructor are:
  \describe{
    \item{\code{name}}{A character scalar identifying the model.}
    \item{\code{growthModels}}{A list of
      \code{\linkS4class{GrowthModel}}s or
      \code{\linkS4class{TransitionModel}}s corresponding to acations.}
    \item{\code{actions}}{A \code{\linkS4class{Panmat}} giving the
      indexes of the population model to use for each student.}
    \item{\code{tname}}{A character vector giving the names of the
      latent variables.  Needs to match the quadrature used.}
    \item{\code{w}}{A character value giving the name of the weight
      column.  Only the first value is used.  Again, this needs to match
      what is in the quadrature.}
  }
}
\section{Slots and Active Fields}{
  \describe{
    \item{\code{$name}:}{A \code{"character"} scalar giving the
      a name for the collection. }
    \item{\code{$models}:}{A \code{"list"} of
      \code{\linkS4class{PopulationModel}} (\code{\linkS4class{FModel}})
      objects containing the actual models.}
    \item{\code{$index}:}{Object of class \code{"Panmat"} giving the
      indexes of which model is used by which (subject, occasion)
      combination.}
    \item{\code{$iname}:}{String giving the name of the index, defaults
      to \dQuote{action}.}
    \item{\code{$action(isubj,iocc)}:}{Returns the index of the growth
      model used for this subject occassion pair.}
    \item{\code{$tnames}:}{A \code{"character"} vector giving the names
      of the latent variables. }
    \item{\code{$wname}:}{A \code{"character"} vector giving the name(s) of
      the weight columnn. }
    \item{\code{$dnames}:}{A \code{"character"} vector giving the name(s)
      of the data column.  Not used.}
    \item{\code{$dt}:}{A \code{\linkS4class{Panmat}} giving the time
      differences.}
    \item{\code{$deltaT(isubj,iocc,value)}:}{Gets (if value is missing)
      or sets the time difference for a specific subject at a specific
      occaion.}
    \item{\code{$dosage}:}{A \code{\linkS4class{Panmat}} giving the
      dosages, or \code{NULL} to indicate that \code{$dt} should be used
      as the dosage.}
    \item{\code{$dose(isubj,iocc,value)}:}{Gets (if value is missing)
      or sets the dosage for a specific subject at a specific
      occaion.}
    \item{\code{$dtname}:}{The name of the data column containing the
      time difference.}
    \item{\code{$dosname}:}{The name of the data column containing the
      dosage.}
  }

  
}
\section{Extends}{

  \code{Activities} extends class \code{"\linkS4class{ModelSet}"},
  directly.  \code{ActivitiesD} extends \code{Actvities}.

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
    \item{\code{nmodels}}{Returns the number of models.}
    \item{\code{as_longform}}{\code{signature(x = "ModelSet")}: Returns
      longform (\code{link{as_longform}}) data consisting of the group
      variable. }
    \item{\code{drawGrowth},\code{$drawNext}}{\code{(model, isubj, iocc,
	theta, covar=NULL)}, calls the
      \code{\linkS4class{GrowthModel}$drawNext} method with the
      model with referenced by \code{$action(isubj,iocc)}.}
  }

  Common arguments:
  \describe{
    \item{model}{The population model, omitted for the R6 (\sQuote{$})
      methods.}
    \item{isubj}{The index of the subject.}
    \item{iocc}{The index of the subject.}
    \item{theta}{Latent variable values for the subject.}
    \item{covar}{Covariates for the subject.}
  }


  Some additional object methods:

  \describe{
    \item{\code{$toString}}{Generates a charcter representation.}
    \item{\code{$print}}{Prints the object; default method uses
      \code{$toString} so only that needs to be modified.}
    \item{\code{$prepData}}{Prepares data for the mstep calculations,
      Adds the deltaT and dose columns.}
    \item{\code{$split_m}}{Assigns data to models and builds a list of
      (model, data) pairs.}
    \item{\code{$mstep}}{\code{(data = "Panel_Data",...,
	its = "integer", control = "list", workers = "Workers")}:
      This is the implementation of the m-step.  Generally only the
      \code{$prepData} method needs to be overridden.}
  }

  Methods used only by \code{ActivitiesD}.

  \item{\code{advanceWeights},\code{$advance}}{\code{(model, isubj,
      iocc, lweights, covar=NULL)}:  Calculates updated left weights in
    the forward part of the Baum--Welch algorithm
    (\code{\link{BWFilter}}).}
  \item{\code{retreatWeights},\code{$retreat}}{\code{(model, isubj,
      iocc, lweights, covar=NULL)}:  Calculates updated right weights in
    the reverse part of the Baum--Welch algorithm
    (\code{\link{BWFilter}}).} 
  \item{\code{$tmat}}{\code{(isubj,iocc,covar)}:  calculates the
    transition matrix for a given subject, occasion pair.}
  \item{\code{$fillCache}}{\code{(workers=Workers$new())}: Does some
    caching of the matrix construction, to make scoring more
    efficient.}
  \item{\code{$nmoments}}{\code{(value)}: The number of matrix
    multiplications do to in \code{\link{expLambdaT}}.}

}
\section{Time and Dosage}{

  Almond (2007) proposes two time frames for models representing student
  learning:  calendar time and rehearsal time or dosage.  Calendar time
  represents the time elapsed between the start and end of the activity
  selection period; this is called \dQuote{deltaT} in \code{fanel}.  The
  \dQuote{dose} represents the amoung of time the subject exercises with
  the skill to be learned (or number of attempts at some activity).

  In general, increases in skills are associated with dosage and
  decreases with calendar time (\dQuote{deltaT}).

}
\section{ActivitiesD and Transition Models}{

  Although the \code{\linkS4class{TransitionModel}} is a subclass of the 
  \code{\link{GrowthModel}}s, it differs in a number of ways.  First,
  the transition model assumes that the latent variable space is
  discrete, and therefore the growth can be represented with a
  transition matrix.  Second, growth models are designed to be used with
  the \code{\link{particleFilter}} algorithm, and so only need the
  \code{\link{drawGrowth}} function, the transition models are designed
  to work with the \code{\link{bwFilter}} and hence need the
  \code{\link{advanceWeights}} and \code{\link{retreatWeights}}
  functions.

  The \code{ActivitiesD} class (\sQuote{D} for discrete) are designed to
  work with \code{\link{TransitionModels}} and have some extra methods
  for supporting this.

}
\section{Making and Caching Matrixes}{

  In continuous time Markov processes, calculating the transition matrix
  is a bit expensive.  It involves a matrix exponential
  (\code{\link{expLambdaT}}) which can be expensive to calculate.  This
  is different for all values of time and dosage.  Hence,
  \code{\linkS4class{TransitionModel}} objects cache the calculated
  transition matrixes using a \code{\linkS4class{memoTree}}.

  During the scoring process if two subjects--occasion pairs have the
  same \code{deltaT} and \code{dose}, the second pair will be able to
  use the cached code.  However, if the computations are done on
  different threads, they will not be able to share the cache.  The
  \code{$fillCache}} method precalculates the caches, using multiple
  threads if \code{\linkS4class{Worker}} are supplied.

}
\references{
  Almond, R. G. (2007). Cognitive Modeling to Represent Growth
  (Learning) Using Markov Decision Processes. \emph{Technology, Instruction,
  Cognition and Learning (TICL)}, \bold{5},
313–324. \url{http://www.oldcitypublishing.com/TICL/TICL.html}
}
\author{Russell Almond}
\note{

  The \code{$tname} and \code{$wname} fields must be co-ordinated with
  the \code{\linkS4class{Quadrature}} class.  In particular, the
  \code{\linkS4class{BWQuad}} has weights \code{c("w.full", "w.left",
    and "w.right")} (only the first is needed for population models)
  while the other quadratrues use just \code{"w"}.

}
\seealso{

  \code{\linkS4class{POMDP}} is the class which holds the
  \code{Activities} or \code{ActivitiesD} class.

  \code{\linkS4class{GrowthModel}} is the abstract class models
  contained by \code{Activities}.  \code{\linkS4class{BrownianGrowth}} and
  \code{\linkS4class{BrownianGrowth2}} are the current implementing
  classes.  These are used with the \code{\link{particleFilter}}
  algorithm. 

  \code{\linkS4class{TransitionModel}} is the abstract class models
  contained by \code{ActivitiesD}.  \code{\linkS4class{UpDownGrowth}} is
  the current implementing class.  These are used with the
  \code{\link{bwFilter}} algorithm.  The \code{\linkS4class{memoTree}}
  is used to cache transition matrixes.
  
}
\examples{
acts <- Activities$new("ABtest",
                       growthModels=list(BrownianGrowth("A",.1,.05),
                                         BrownianGrowth("B",.2,.1)),
                       actions=matrix(c(1,1,2,2, 2,2,1,1,),2,4,byrow=TRUE),
                       dt=1,dosage=c(1,2,1,2))
nmodels(acts)
}
\keyword{classes}
