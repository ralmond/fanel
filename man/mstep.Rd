\name{mstep}
\alias{mstep}
\alias{lprob}
\alias{pvec}
\alias{pvec<-}
\title{Generic functions for implemented the M-step}
\description{

  The \code{mstep} generic function does the M-step of the EM-algorithm
  for estimating parameters of the models.  The \code{pvec} function
  gets and sets the parameters.  The \code{lprob} function calculates
  the log posterior (likelihood) of the data given specified parameters,
  and the default mstep does the maximization.

}
\usage{
mstep(obj, data, ..., its = 3, control = list(), workers = Workers$new())
lprob(obj, data, par=pvec(obj))
pvec(obj)
pvec(obj) <- value
}
\arguments{
  \item{obj}{A \code{\linkS4class{FModel}},
    \code{\linkS4class{ModelSet}} or one of their subclasses, as well as
    the \code{\linkS4class{POMDP}} class.}
  \item{data}{A data frame providing the data, see the data section
    below.}
  \item{par,value}{A vector of parameters.  See the parameters section below.}
  \item{\dots}{A separator between optional and required parameters}
  \item{its}{The default algoritm runs the \code{\link[stats]{optim}}
    optimizer for \code{its} cycles.}
  \item{control}{The default algorithm runs the
    \code{\link[stats]{optim}}; the \code{control} parameter is passed
    to \code{\link[stats]{optim}}.}
  \item{workers}{A \code{\linkS4class{Workers}} object used to support
    multiprocessing.}
}
\details{

  The parameters of a POMDP model are located within each of the
  \code{\linkS4class{FModel}} (the \code{pvec} function accesses them).
  These can be adjusted to fit the data using the EM-algorithm
  (Dempster, Laird and Rubin, 1977).  This algorithm alternates between
  two phases:  the E-step (expectation) where given the current values
  of the parameters, the expected value of the log posterior (or
  likelihood) is calculated, and the M-step (maximization) where new
  parameters are calculated with maximize the expected log posterior.

  \emph{E-step}.  The role of the E-step is to calculate the expected
  value of the log posterior based on the current parameter values.  The
  basic filtering functions,  \code{\link{particleFilter}},
  \code{\link{BWFilter}} and even \code{\link{qFilter}}, all return a
  quadrature with its weights filled in.  These give an approximate
  posterior distribution for the latent variable(s) theta.  This gets
  bound with the input data into the \code{data} object, which is then
  passed to the \code{mstep} (see the section on Data below).

  \emph{M-step}.  Fortunately, the optimization can be split across the
  various models with the various \code{\linkS4class{FModel}}.  Meng and
  van Dyk (1997) call this a structural EM algorithm.  So the
  \code{mstep(\linkS4class{POMDP})} method calls the
  \code{mstep(\linkS4class{ModelSet}} methods, which split the data
  according to which model was used, and then call the methods on the
  various models which do the actual maximization.

  The default \code{$mstep} method, implemented by the
  \code{\linkS4class{FModel}} class, calls \code{\link[stats]{optim}} to
  optimize \code{\(par) {$lprob(data,par)}}.  The \code{control}
  argument is passed to \code{optim} and can be used to turn on
  debugging information.  The \code{its} argument is added to control,
  and controls how many rounds the \code{optim} algorithm runs.
  According Dempster, Laird and Rubin (1977) the M-step doesn't need to
  be run to completion.  This becomes a tuning parameter.

  In some cases, the optimization problem can be solved numerically, and
  the iterative solution is not needed.  In these cases the model can
  override the \code{$mstep} function.

}
\value{

  The \code{mstep} method for a basic \code{\linkS4class{FModel}}
  returns a list with two elements, the name of the model, and some
  information about convergence (by default the results from
  \code{optim}).  The \code{ModelSet} and \code{POMDP} methods return a
  list of such results.

  The \code{lprob} methods return a single real value giving the log
  posterior.

  The \code{pvec} methods return a vector of real numbers, but the
  length and interpretations depend on the actual model.

}
\section{Data}{

  At the transition between the E-step and M-step a complete data file
  is built using the call \code{\link{longform}(quad,model,data)}, which
  joins the longforms of the \code{\linkS4class{Quadrature}},
  \code{\linkS4class{POMDP}} and the \code{\linkS4class{Panel_Data}}.  This
  results in a long form data set with indexes \dQuote{subj},
  \dQuote{occ}, and \dQuote{quad}.

  Here is a complete list of the columns in the combined data set.  The
  names can be changed by overriting the fields after the names.

  \describe{
    \item{\dQuote{subj}}{The index for the subject (positive integer) }
    \item{\dQuote{occ}}{The index for occasion (non-negative integer) }
    \item{\dQuote{quad}}{The indexe for quadtrature point }
    \item{\dQuote{theta}, \code{quad$tnames}}{The name for the latent
      variable(s); could be more than one column. }
    \item{\dQuote{theta_1}, \code{paste0(quad$tnames,"_1")}}{The value
      of theta(s) one time point ahead of the current occastion (added by
      the \code{mstep} method for \code{\linkS4class{Activities}}). }
    \item{\dQuote{w},\dQuote{w.full}, \dQuote{w.left}, \dQuote{w.right},
      \code{quad$wname}}{The column of weights. Note that the
      the \code{\link{BWQuad}} provides three different weights, so for
      \code{\linkS4class{PopulationModel}}s and
      \code{\linkS4class{EvidenceModel}}s the proper reference is
      \code{$wname[1]}.}
    \item{\dQuote{group},\code{Population$iname}}{The group index for
      \code{\linkS4class{PopulationModel}}s.}
    \item{\dQuote{task},\code{Evidence$iname}}{The task index for
      \code{\linkS4class{EvidenceModel}}s. }
    \item{\dQuote{action},\code{Activities$iname}}{The activity index for
      \code{\linkS4class{GrowthModel}}s. }
    \item{\dQuote{deltaT},\code{Activities$dtname}}{The difference in
      time between each pair of occasions. }
    \item{\dQuote{dose},\code{Activities$dosname}}{The usage of the
      activity between each }
    \item{\dQuote{Y},\code{Evidence$dnames}}{The data column(s).}
    \item{\dots}{Other covariate columns.}
  }

  The \code{$split_m} method of \code{\linkS4class{ModelSet}} classes
  (\code{\linkS4class{Population}}, \code{\linkS4class{Evidence}}, and
  \code{\linkS4class{Actvities}}) split the data by their indexes and
  sends the chunks to the models in the collections.

}
\section{Parameters}{

  Parameters are stored in two different ways.  First as fields in the
  \code{\linkS4class{FModel}} object, second as the \code{pvec} active
  property, which must be implemented for each subclass.  As the
  \code{pvec} is passed to \code{\link[stats]{optim}} it must be a
  vector, and it is best if the parameters are transformed so that their
  domain is the whole real line.

  For example, the \code{\linkS4class{NormalPop}} class has two parameters
  \code{$mu} and \code{$sigma}, however, the parameter vector uses
  \code{c($mu,log($sigma)} to move the domain of the second parameter
  from the positive numbers to the real line.  The
  \code{\linkS4class{CategoricalPop}} class has natural parameters
  \code{$probs} which give the probability of each category (quadrature
  point).  The parameter vector is the log of the probabilities, which is
  transfered back to the natural scale using the softmax function.

}
\section{Algorithm}{

  After the E-step computes a new quadrature,
  \code{\link{longform}(quad,model,data)} computes the grand
  and glorius full data frame.

  Next, the \code{mstep(\linkS4class{POMDP})} method passes the data
  set to \code{$split_m} of the \code{\linkS4class{Population}},
  \code{\linkS4class{Evidence}}, and \code{\linkS4class{Activities}}
  classes to build a list of (model, data) pairs.  The \code{$split_m}
  method first calls the \code{$prepData} method to do some
  pre-processing.  For the \code{Population} class, this filters the
  data for only occasion 0; for the \code{Activities} class, this adds
  the \dQuote{theta_1} columns.  The \code{mstep(POMDP)} method then
  calls all of the individual model \code{FModel$mstep} methods with
  their data. 

  The default \code{mstep(\linkS4class{FModel})} method runs \code{optim}
  for \code{its} steps, however, specific models can implement exact methods
  if available.

  The lowest level \code{mstep} methods return a list giving the name of the
  model and some details of the results.  The higher level models return
  a list of such lists.

  The \code{lprob(\linkS4class{FModel})} method returns a single
  log probability.  The \code{lprob(\linkS4class{ModelSet})} returns
  the sum of the log probabilities which can be used to monitor
  convergence.

}
\section{Multiprocessing Support}{

  The \code{workers} argument is used to split th processing across
  worker threads (see \code{\linkS4class{Workers}}).

  The \code{workers} argument is ignored with the
  \code{\linkS4class{FModel}} classes.

  The \code{\linkS4class{ModelSet}} class uses the worker class to split
  the \code{mstep} operation for the component models.  The default
  method tries to generate as many workers as it contains models.  The
  \code{\linkS4class{POMDP}} method first calls the \code{$split_m}
  methods on the model sets to build a combined list of (model,data)
  pairs.  It then uses the worker class to split this processing among
  the available pool of workers.

}
\references{

Dempster, A. P., Laird, N., & Rubin, D. B. (1977). Maximum likelihood
from incomplete data via the EM algorithm. \emph{Journal of the Royal
  Statsitical Society, Series B}, \bold{39}, 1–38.

Meng, X.-L., & van Dyk, D. (1997). The EM Algorithm—An Old Folk-song
Sung to a Fast New Tune. \emph{Journal of the Royal Statsitical Society,
  Series B}, \bold{59}(3), 511–567.

}
\author{Russell Almond}
\note{

  Obviously, \code{its} is a tuning parameter and will need some tuning.
  It may eventually require some kind of scheduling.

}
\seealso{

  \code{\linkS4class{POMDP}} --- Big Model container

  \code{\linkS4class{ModelSet}} --- Next level model containers.
  Subclasses:  \code{\linkS4class{Population}},
  \code{\linkS4class{Evidence}}, and \code{\linkS4class{Actvities}})

  \code{\linkS4class{FModel}} --- Low level model:  Subclasses
  \code{\linkS4class{PopuluationModel}} (\code{\linkS4class{NormalPop}},
  \code{\linkS4class{CategoricalPop}}), \code{\linkS4class{GrowthModel}}
  (\code{\linkS4class{TransitionModel}}),
  \code{\linkS4class{EvidenceModel}} and their subclasses.

  \code{\link[stats]{optim}} --- Does the work in the default method.

  \code{\linkS4class{Workers}} --- Used to divide the workload among
  threads.

  \code{\link{longform}(quad,model,data)} -- which puts the data together.

  \code{\linkS4class{Quadrature}} -- output of E-step, which is one of
  the funcitons:  \code{\link{particleFilter}}, \code{\link{BWFilter}},
  or \code{\link{qFilter}}

  \code{\linkS4class{Panel_Data}} -- the basic data and covaraites.


}
\examples{

}
\keyword{ optimize }
\keyword{ methods }
\concept{ panel_data }
\concept{ quadrature }
\concept{ EM algorithm }

