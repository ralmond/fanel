\name{HMM}
\Rdversion{1.1}
\docType{class}
\alias{Workers-class}
\alias{Workers}
\title{An object for encapsulating parallel coding}
\description{

  This object wraps a \code{parallel::cluster}
  (\link[parallel]{makeCluster}) allowing a bit more control.
  In particular, it runs in a single threaded mode when debugging.

}
\section{Public Fields}{

  \describe{
    \item{cltype}{The type argument of
      \code{\link[parallel]{makeCluster}}.  The value \code{"FORK"}
      is preferred for *nix, if you have windows your only choice is "PSOCK".}
    \item{clspec}{The first argument to
      \code{\link[parallel]{makeCluster}}, which is usually the number
      of cores to use.}
    \item{clargs}{Additional arguments to \code{\link[parallel]{makeCluster}}}
    \item{stopClusterOnError}{A logicial value.  If \code{TRUE}, the
      cluster will be closed on an error.}
    \item{seed}{A random number generator seed, passed on to the
      cluster.}
    \item{debug}{A logical value.  If true, then all work will be done
      in the current R instance and not in a worker, so that the
      debugger can be run.}
   }
}
\section{Public Methods}{

  \describe{
    \item{initialize}{\code{signature(clspec,clargs,seed,
	stopClusterOnError,debug,type)}:  Called through \code{$new}.
      All parameters are optional.}
    \item{lapply,sapply}{\code{signature(...)}:  runs either the named
      function or the
      \code{parallel::\link[parallel]{parLapply}}/\code{parSapply}
      parallel variant.}
    \item{start}{\code{signature(\var{envir})}: creates the cluster
      object.  If the \code{envir} argument is supplied, it should
      either be an environment which when exited the cluster should
      stop, or \code{NULL} indicating the cluster must be stopped
      manually.}
    \item{stop}{\code{signature()}: stops the cluster object if it was
      started.  It is safe to call multiple times.}
    \item{flagStop}{\code{signature()}:  Sets a flag so the cluster
      will be stopped on exit.}
    \item{condStop}{\code{signature()}:  Stops the cluster if the flag
      has been set either through \code{$flagStop()} or because the
      code \code{$stopClusterOnError} flag was set the cluster was
      started.}
  }

}
\section{Constructor}{

  To create a new worker cluster, use the expression
  \code{Workers$new(\var{clspec},\var{clargs},\var{seed},
    \var{stopClusterOnError},\var{debug},\var{type})}.  The \code{type}
  argument defaults to \dQuote{FORK} on POSIX machines and defaults to
  \dQuote{SOCK} on Windows.

  In the simplest use case,
  \code{$clspec} is the number of cores to use.  I recomend
  using \code{parallel::detectCores()-2} (leaving one core for RStudio
  and one for your operating system).  If unspecified it defaults to
  \code{\link[base]{options}("mc.cores")} or 2 if that option is unspecified.

}
\section{Parallel Computations}{

  The \code{$start()} function calls
  \code{\link[parallel]{makeCluster}} with the arguments
  \code{($clspec,$cltype,$clargs)}.  See the docs for
  \code{\link[parallel]{makeCluster}} for running across multiple
  machines.  If \code{debug} is true, then the cluster is not started
  and the loop is run in the main process (this facilitates debugging).

  The methods \code{$lapply(...)} and \code{$sapply(...)} behave like the base
  versions.  In particular, if the cluster was started, they use
  \code{\link[parallel]{parLapply}} and
  \code{\link[parallel]{parSapply}}, if not, they use the base versions.

  The function \code{$stop()} stops the cluster, unlike the
  \code{\link[parallel]{stopCluster}}, it can be safely called on an
  already stopped cluster.  The cluster needs to be
  restarted before it can be used again.

  The function \code{$condStop()} stops conditionally if the stop flag
  has been set.  The \code{$condStop()} function is designed to be
  called within a call to \code{withr::\link[withr]{defer}}.  This flag
  is set during the call to \code{$start()} if
  \code{$stopClusterOnError} is true (the default).  To leave the
  cluster intact for debugging, set this field to false.  The method
  \code{$flagStop()} sets the flag so that the \code{$condStop()} will
  stop the cluster when the environment exits.

  The \code{$start(envir=parent.frame())} function, if the \code{envir}
  argument is non-null, includes a call to
  \code{withr::\link[withr]{defer}(self$condStop(), envir=envir)}, which
  will stop the cluster when the function which called \code{$start()}
  exits.  If this is not the same function as the main loop, then the
  \code{envir} might need to be set to a parent frame, or it can be set
  to \code{NULL} to suppress the call to \code{defer}.

}
\section{Superclasses and Subclasses}{

  This is an instance of \code{\link[R6]{R6Class}}.

}
\author{Russell G. Almond}
\seealso{

  \code{\link[parallel]{makeCluster}},
  \code{\link[parallel]{parLapply}}, \code{\link[base]{lapply}},
  \code{\link{particleFilter}}, \code{\link{simulate.POMDP}},

}

\examples{

\dontrun{

    ## Leave 2 cores for other jobs (e.g., display manager)
    w <- Workers$new(max(getOption("mc.cores",2L)-2L,1L),
                     stopClusterOnError=TRUE)
    ## Uncomment this line to do debugging
    ## w$debug <- TRUE


  processCases <- function(caseList, workers=w) {
    w.lapply(caseList,\(x) {commands})
    w$flagStop() # So cluster will close when function exits.
  }

}

}
\keyword{iteration}
