\name{Population-class}
\docType{class}
\alias{Population-class}
\alias{Population}
\alias{as_longform,Population-method}

\title{Class \code{"Population"}}
\description{

  A class that contains a collection of
  \code{\linkS4class{PopulationModel}} objects indexed by
  \dQuote{group}.

}
\section{Constructor}{

  The constructor is \code{Population$new(name, popModels, groups=1L,
    tname="theta", wname="w")}.

  The arguments for both constructors are:
  \describe{
    \item{\code{name}}{A character scalar identifying the model.}
    \item{\code{popModels}}{A list of
      \code{\linkS4class{PopulationModel}}s corresponding to groups.}
    \item{\code{groups}}{A \code{\linkS4class{Panmat}} giving the
      indexes of the population model to use for each student.}
    \item{\code{dt}}{A \code{\linkS4class{Panmat}} giving the
      time differences between each occasion.}
    \item{\code{dosage}}{A \code{\linkS4class{Panmat}} giving the
      exposure time/strenght between each occasion.  If \code{NULL},
      then the time difference will be used.}
    \item{\code{tname}}{A character vector giving the names of the
      latent variables.  Needs to match the quadrature used.}
    \item{\code{wname}}{The column names for the weights.  For
      \code{Activities}, only the first value is used.  For
      \code{ActivitiesD} this should be a vector of length three with
      the second element being the left weights and the third the right
      weights. Again, this needs to match what is in the quadrature.}
    \item{\code{dtname}}{A character value giving the name of the time
      difference column.}
    \item{\code{dosname}}{A character value giving the name of the time
      dosage column.}
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
      to \dQuote{group}.}
    \item{\code{$tnames}:}{A \code{"character"} vector giving the names
      of the latent variables. }
    \item{\code{$wname}:}{A \code{"character"} vector giving the name(s) of
      the weight columnn. }
    \item{\code{$dnames}:}{A \code{"character"} vector giving the name(s)
      of the data column.  Not used.}
  }
}
\section{Extends}{

  Class \code{"\linkS4class{ModelSet}"}, directly.

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
    \item{\code{drawInitial},\code{$drawInit}}{\code{(model, isubj,
	npart, covar=NULL)}, calls the
      \code{\linkS4class{PopulationModel}$drawInit} method with the
      model with referenced by \code{$group(isubj)}.}
    \item{\code{probInit},\code{$initProbs}}{\code{(model, isubj,
	theta, covar=NULL)}, calls the
      \code{\linkS4class{PopulationModel}$initProbs} method with the
      model with referenced by \code{$group(isubj)}.}

  }

  Common arguments:
  \describe{
    \item{model}{The population model, omitted for the R6 (\sQuote{$})
      methods.}
    \item{isubj}{The index of the subject.}
    \item{theta}{Latent variable values for the subject.}
    \item{covar}{Covariates for the subject.}
  }


  Some additional object methods

  \describe{
    \item{\code{$toString}}{Generates a charcter representation.}
    \item{\code{$print}}{Prints the object; default method uses
      \code{$toString} so only that needs to be modified.}
    \item{\code{$prepData}}{Prepares data for the mstep calculations,
      filters to only include occasion 0.}
    \item{\code{$split_m}}{Assigns data to models and builds a list of
      (model, data) pairs.}
    \item{\code{$mstep}}{\code{signature(data = "Panel_Data",...,
	its = "integer", control = "list", workers = "Workers")}:
      This is the implementation of the m-step.  Generally only the
      \code{$prepData} method needs to be overridden.}
  }

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
  \code{PopulationClass}.

  \code{\linkS4class{PopulationModel}} is the abstract class for
  supported models.  \code{\linkS4class{NormalPop}} and
  \code{\linkS4class{CategoricalPop}} are the current implementing
  classes.

}
\examples{
apop <- Population$new("BigStudy",
                       list(NormalPop$new("Reference",0,1),
                            NormalPop$new("Focal",-.5,1.2)),
                       groups=c(rep(1,7),rep(2,3)),
                       tname="Ability",
                       wname="w")
nsubj(apop)
nmodels(apop)
drawInitial(apop,1,5)
drawInitial(apop,9,5)
probInit(apop,1,c(-1,0,1))

}
\keyword{classes}
\keyword{models}
\concept{panel_data}
\concept{population_model}
\concept{ POMDP}
\concept{ HMM}

