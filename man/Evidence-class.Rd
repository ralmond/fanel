\name{Evidence-class}
\docType{class}
\alias{Evidence-class}
\alias{Evidence}
\alias{as_longform.Evidence}
\alias{drawData.Evidence}
\alias{evalEvidence.Evidence}

\title{Class \code{"Evidence"}}
\description{

  A container class for \code{\linkS4class{EvidenceModel}}s, indexed by
  \dQuote{task}.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Constructor}{

  The constructor is \code{Evidence$new(name, evidenceModels,
    tasks=matrix(1L,1L,1L), tname="theta", wname="w", dname="Y")}.

  The arguments for the constructor are:
  \describe{
    \item{\code{name}}{A character scalar identifying the model.}
    \item{\code{evidenceModels}}{A list of
      \code{\linkS4class{EvidenceModel}}s corresponding to groups.}
    \item{\code{groups}}{A \code{\linkS4class{Panmat}} giving the
      indexes of the population model to use for each student.}
    \item{\code{tname}}{A character vector giving the names of the
      latent variables.  Needs to match the quadrature used.}
    \item{\code{w}}{A character value giving the name of the weight
      column.  Only the first value is used.  Again, this needs to match
      what is in the quadrature.}
    \item{\code{tname}}{A character vector giving the names of the
      data variables.  Needs to match the data used.}
  }
}
\section{Slots and active fields.}{
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
      of the data column.}
  }
}
\section{Extends}{

  Class \code{"\linkS4class{ModelSet}"}, directly.

}
\section{S4 Methods and class methods}{
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
    \item{\code{drawData}, \code{$drawObs}}{\code{(model,isubj,iocc,theta,
        cov=NULL)}: Draws random values for data values.  Used in
      \code{\link{simulate.POMDP}}. }
    \item{evalEvidence}{\code{(model,isubj,iocc,theta,Y,covar=NULL)}:
      Calculates the log-likehood of the data given the latent variable.}
  }

  Common arguments:
  \describe{
    \item{model}{The population model, omitted for the R6 (\sQuote{$})
      methods.}
    \item{isubj}{The index of the subject.}
    \item{iocc}{The index of the occasion.}
    \item{theta}{Latent variable values for the subject,occasion}
    \item{Y}{Data variable values for the subject, occasion}
    \item{covar}{Covariates for the subject, occasion.}
  }

    Some additional object methods

  \describe{
    \item{\code{$toString}}{Generates a charcter representation.}
    \item{\code{$print}}{Prints the object; default method uses
      \code{$toString} so only that needs to be modified.}
    \item{\code{$prepData}}{Returns data, but could be overridden if
      needed by a subclass.}
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

  The \code{$dname} field must be co-ordinated with the
  \code{\linkS4class{Panel_Data}} object.

}
\seealso{
  \code{\linkS4class{POMDP}} is the class which holds the
  \code{PopulationClass}.

  \code{\linkS4class{EvidenceModel}} is the abstract class for
  supported models.  \code{\linkS4class{NormalScore}} and
  \code{\linkS4class{GradedResponse}} are the current implementing
  classes.
}
\examples{
ems <- Evidence$new("BigStudy",
                    list(easy=NormalScore$new("ns",-1,1/25),
                         hard=NormalScore$new("ns",1,1/25)),
                    tasks=matrix(c(rep(1,3),rep(2,3)),1,6),
                    tname="Ability",wname="w",dname="Score")
nsubj(ems)
nmodels(ems)
theta <- -2:2
drawData(ems,1,2,theta)
drawData(ems,2,4,theta)
evalEvidence(ems,1,2,theta,1)
evalEvidence(ems,2,4,theta,1)
}
\keyword{classes}
\keyword{models}
\concept{ panel_data }
\concept{ POMDP}
\concept{ HMM}
\concept{evidence_model}
