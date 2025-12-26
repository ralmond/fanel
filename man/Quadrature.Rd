\name{Quadrature-class}
\docType{class}
\alias{Quadrature-class}
\alias{Quadrature}
\alias{FixedQuad-class}
\alias{FixedQuad}
\alias{fixedQuad}
\alias{ParticleQuad-class}
\alias{ParticleQuad}
\alias{particleQuad}
\alias{BWQuad-class}
\alias{BWQuad}
\alias{bwQuad}
\alias{as_longform.Quadrature}
\alias{get_subj.Quadrature}
\alias{get_subj<-.Quadrature}
\alias{maxocc.Quadrature}
\alias{maxocc<-.Quadrature}
\alias{minocc.Quadrature}
\alias{minocc<-.Quadrature}
\alias{nocc.Quadrature}
\alias{nocc<-.Quadrature}
\alias{nquad,Quadrature}
\alias{nquad<-.Quadrature}
\alias{nsubj.Quadrature}
\alias{nsubj<-.Quadrature}
\alias{isubj.Quadrature}
\alias{isubj<-.Quadrature}

\title{Class \code{"Quadrature"}}
\description{

  This object contains a set of quadrature points (called
  \dQuote{thetas}) and a series of weights for each subject and
  occasion.  The approximate value at each time point is the weighted
  average of the \dQuote{thetas} at each time point.

}
\section{Objects from the Class}{

  \code{Quadrature} is a virtual class: No objects may be created from
  it.  It is also an \code{\link[R6]{R6Class}}, so changing the elements
  changes the object in multiple places.

  There are three principle sub-classes:

  \code{FixedQuad} which has constructor:
  \code{fixedQuad(times,qpoints,tnames=vnames(qpoints),
    static=TRUE, bysubj=(nsubj(times)>1L),
    nsubjects=nsubj(times),isubj=NA_integer_)}.
  This variant has a fixed quadrature which is the same for all
  occasions. This is designed to be
  used with the \code{\link{qFilter}} function.

  \code{BWQuad} which has constructor:
  \code{BWquad(times,qpoints,tnames=vnames(qpoints),
    static=TRUE, bysubj=(nsubj(times)>1L),
    nsubjects=nsubj(times),isubj=NA_integer_)}.  This also has a fixed
  quadrature, but it has but both forward and backwards weights.  This
  is designed to be used with the Baum-Welch filter
  \code{\link{bwFilter}}  function.


  \code{ParticleQuad} which has constructor:
  \code{fixedQuad(times,nquadrature,tnames=vnames(qpoints), static=TRUE,
  bysubj=(nsubj(times)>1L), nsubjects=nsubj(times),isubj=NA_integer_)}.
  This is a random quadrature whose points (particles) move between each
  occaions.  This is designed to be used with the
  \code{\link{particleFilter}} function.

}
\section{Slots and Active Fields}{
  \describe{
    \item{\code{static}:}{A logical value.  If true then the quadrature
      points are the same for each occasion.}
    \item{\code{bySubj}:}{A logical value.  If false then the quadrature
      points are different for each subject.}
    \item{\code{wname}:}{The name to use for the weight object.}
    \item{\code{lweights}:}{A \code{subj} x \code{occ} x \code{quad}
      array giving the log weights.  See the \code{$lweight()} function
      for accessing parts of the array.}
    \item{\code{thetas}:}{The full \code{nsubj} x \code{nocc} x
      \code{nquad} x \code{dtheta} array of quadrature points.  See the
      \code{$theta()} function below for accessing part of the array.}
    \item{\code{tnames}:}{A \code{"character"} vector containing the names
      for the quadrature variables.  Should have length \code{$dtheta}}
    \item{\code{dtheta}:}{An \code{"integer"} containing the number of
      dimenions for each quadrature point.}
    \item{\code{times}:}{Object of class \code{"Panmat"} mapping
      measurement occasions to times.}
    \item{\code{minocc}:}{An \code{"integer"} giving the index for the
      smallest index in this quadrature.}
    \item{\code{maxocc}:}{An \code{"integer"} giving the index for the
      largest index in this quadrature.}
    \item{\code{nsubj}:}{An \code{"integer"} giving the number of
      subjects in this quadrature.}
    \item{\code{isubj}:}{An integer giving the index the subject which
      is the focus of this quadrature (see \code{\link{split_subj}}).}
    \item{\code{nquad}:}{An \code{"integer"} giving the number of
      quadrature points.}
  }
}
\section{Class Methods}{
  \describe{
    \item{$weights(type="default")}{Returns normalized weights (on probability,
      not log scale).}
    \item{$resetWeights()}{Clears the weights before re-running filter.}
    \item{$theta(subj,occ,quad,value)}{Gets (if \code{value} is missing)
      or sets the quadrature points for a given subject, occasion.  The
      \code{quad} field can be missing to set all quadrature points.}
    \item{$lweight(subj,occ,quad,value)}{Gets (if \code{value} is missing)
      or sets the log weights for a given occasion. Again, \code{quad}
      can be omitted.}
  }

  Note that my intention was to allow expressions of the type
  \code{quad$lweight(subj,occ,) <- value} and similarly with
  \code{$theta}. However, \code{quad$lweight(subj,occ,,value)}.

}
\section{Extends}{
Class \code{"\link[R6]{R6Class}"}, directly.
}
\section{Methods}{
  \describe{
    \item{as_longform}{\code{signature(x = "Quadrature")}: Converts the
      quadrature into very long form (with indexes subj, occ and quad). }
    \item{maxocc}{\code{signature(obj = "Quadrature")}: Gets the maximum
      occasion index.}
    \item{maxocc<-}{\code{signature(obj = "Quadrature")}: Sets the
      maximum occasion index. }
    \item{minocc}{\code{signature(obj = "Quadrature")}: Gets the minimum
      occasion index.}
    \item{minocc<-}{\code{signature(obj = "Quadrature")}: Sets the
      minimum occasion index. }
    \item{nocc}{\code{signature(obj = "Quadrature")}: Gets the number of
      occasions. }
    \item{nocc<-}{\code{signature(obj = "Quadrature")}: Sets the number
      of occasions.}
    \item{nquad}{\code{signature(obj = "Quadrature")}: Gets the number
      of quadrature points. }
    \item{nquad<-}{\code{signature(obj = "Quadrature")}: Sets the number
      of quadrature points (only works for \code{\link{particleFilter}}).}
    \item{nsubj}{\code{signature(obj = "Quadrature")}: Gets the number
      of subjects. }
    \item{nsubj<-}{\code{signature(obj = "Quadrature")}: Sets the number
      of subjects.}
    \item{isubj}{\code{signature(obj = "Quadrature")}: Gets the index of
      the focus subject (see \code{\link{split_subj}}).}
    \item{isubj<-}{\code{signature(obj = "Quadrature")}: Sets the index
      of the focus subject.}
    \item{get_subj}{\code{signature(x = "Quadrature", subj="integer")}:
      Returns a
      subquadrature focused on a single individual.  See
      \code{\link{split_subj}}. }
    \item{get_subj<-}{\code{signature(x = "Quadrature", subj =
	"integer", value = "Quadrature")}: Replaces
      sub-quadrature focused on a single individual.  See
      \code{\link{split_subj}}. }
  }
}
\details{

  The quadrature is defined by a collection of quadrature points.  These
  are named \dQuote{theta} after the latent variable in Item Response
  Theory models.  Note that theta can be \code{$dtheta}-dimensional, the
  names of the dimensions are the \code{$tnames}.  There is a different
  value for eqch quadrature point.  Potentially, there
  can be a different quadrature values for different subjects and
  occasions as well.  So the \code{$thetas} has dimension \code{$nsubj}
  x \code{$nocc} x \code{$nquad} x \code{$dtheta}.  If \code{$bySubj} is
  false, then the size of the first dimension is 1 (as the points are
  the same for all individuals).  If \code{$static} is true, then the
  size of the second dimension is 1.  The
  \code{$theta(subj,occ,quad,value)} function gets or sets a subset of
  that array.

  Meanwhile there is one log-weight (\code{$lweights}) per particle.
  Note that even if the location of the quadrature points does not
  change with subject and occasion, the weights do.  So the
  size of the array is \code{$nsubj} x \code{$nocc} x \code{$nquad}.
  The \code{$lweight(subj,occ,quad,value)} function gets or sets a
  subset of that array.

  Both the \code{\link{qFilter}} and
  \code{\link{particleFilter}} use log weights as they can be
  calculated with addition and are less susceptible to numerical
  underflow.  The \code{$weights("default")} function takes care of
  this.  The \code{\link{bwFilter}} uses a slightly different scheme
  (see below).  The \code{$wname} gives the name of the weight column.

}
\section{Fixed Quadrature}{

  A fixed quadrature uses the same collection of quadrature time points
  for subject and occasion (the code actually allows differences, but
  not sure if this is useful).  Currently, this filter is designed to
  work with the \code{\link{qFilter}}.  This filter is different
  from the others in that the times for the quadrature does not need to
  be the same as the times for the data.

  The \code{$weights()} function only takes the \code{"default"}
  argument.  The default name for the weights is \dQuote{w}.

}
\section{Baum-Welch Quadrature}{

  This is similar to the fixed quadrature, the difference is in the
  weights.  The Baum-Welch algorithm (\code{\link{bwFilter}})
  requires both weights on the natural scale (as matrix multiplication
  is used in the calculation) and both forwards (\code{$lweights}) and
  backwards (\code{$rweights}) weights.  The \code{$weights()} takes
  four possible values for the \code{type} argument:
  \describe{
    \item{default}{The product of the left and right weights,
      normalized.}
    \item{left}{The left weights, normalized.}
    \item{right}{The right weights, unnormalized.}
    \item{all}{All three weights in a data frame.  Note that the columns
      are named \code{full}, \code{left} and \code{right}, but are
      commonly prefixed with \code{w.} in \code{\link{as_longform}}.}
  }

}
\section{Particle Quadrature}{

  When using the \code{\link{particleFilter}}, both the values of
  the quadrature points and the weights are set by the filter.  Thus the
  quadrature points vary by both subject and occasion, but do not need
  to be pre-specified.  Instead, only the number of quadrature points,
  and the names for the dimensions of the quadrature points needs to be
  specified.

  There are two different weights.  The \dQuote{default} weights use the
  weights from the last equation; these incorporate evidence from all
  observations and activities.  The \dQuote{history} weights show the
  evolution of the weights over the course of the filter; the default
  weights are the right ones for inference, but the history weights
  might be useful for understanding what the filter is doing.

}
\section{Constructors}{

  There is a constructor for each of these types.

  \code{
fixedQuad(times,qpoints,tnames=vnames(qpoints),
          static=TRUE, bysubj=(nsubj(times)>1L),
          nsubjects=nsubj(times),isubj=NA_integer_)
particleQuad(times,nquadrature,tnames,
             static=FALSE, bysubj=(nsubj(times)>1L),
             nsubjects=nsubj(times),
             isubj=NA_integer_)
BWquad(times,qpoints,tnames=vnames(qpoints),
       static=TRUE, bysubj=(nsubj(times)>1L),
       nsubjects=nsubj(times),isubj=NA_integer_)
  }

  Most of the arguments are common:
  \describe{
    \item{times}{A \code{\linkS4class{Panmat}} giving the times at which
      the filter will be evaluated.  For the \code{ParticleQuad} and
      \code{BWQuad} these need to match the observation times in the
      data.}
    \item{qpoints}{An array or data frame giving the location of the
      quadrature points.  See below.}
    \item{nquadrature}{For the \code{ParticleQuad}, the number of
      quadrature points is given.  (For the other two methods it is
      given by the size of the quadrature array.)}
    \item{tnames}{The names of the theta variables.  The default value
      takes the names from the last dimension of the \code{qpoints}
      argument.}
    \item{static}{A logical value.  If true, then the same quadrature
      points should be used for each time point.}
    \item{bySubj}{A logical value.  If false, different quadrature
      points are used for each individual.}
    \item{nsubjects}{The number of subjects covered by the quadrature.}
    \item{isubj}{An integer giving the index of the subject this
      quadrature is focused on if the result of a
      \code{\link{split_subj}} split, or \code{NA} if this is not
      focused on a single subject.}
  }

  The \code{qpoints} argument needs additional explanation.  Eventually,
  it will become a 4-dimensional array with dimensions \code{(nsubj, nocc,
  nquad, dtheta)}.  If \code{bySubj=FALSE} or \code{static=TRUE} then
  the first or second dimension has length 1, and can be omitted.  In
  this case, \code{qpoints} can be a data.frame (which is coerced to a
  numeric array).  If in addition \code{dtheta=1} then \code{qpoints}
  can be a vector.  In this case, it is best to supply the \code{tnames}
  argument.

}
\section{Longform}{

  The \code{\link{as_longform}()} method for \code{Quadrature} generates
  long data with three levels:  \code{subj}, \code{occ}, and \code{quad}
  unlike the other methods, which just have \code{subj} and \code{occ}.
  In many cases, before joining with other data, the analyst will want
  to summarize across quadrature points using \code{\link{summaryq}} or
  \code{\link{summaryqq}}.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{Panmat}} for the time argument.
  \code{\linkS4class{Panel_Data}} and \code{\linkS4class{POMDP}} as other
  objects which need to conform.

  \code{\link{split_subj}} and \code{\link{bind_subj}} for splitting and
  recombining the quadrature by subject.

  \code{\link{qFilter}}, \code{\link{particleFilter}} and
  \code{\link{bwFilter}} for the filtering algorithms which set the
  weights.

  \code{\link{as_longform}}, \code{\link{summaryq}} and
  \code{\link{summaryqq}}.  for summarizing results.

}
\examples{
showClass("Quadrature")
}
\keyword{classes}
\keyword{ ts }
\concept{ panel_data }
\concept{ quadrature }
