Workers <- R6Class(
  "Workers",
  public=list(
    cltype=ifelse(.Platform$OS.type=="windows","PSOCK","FORK"),
    clspec=getOption("mc.cores",2L),
    clargs=list(),
    seed=NULL,
    stopClusterOnError=TRUE,
    debug=FALSE,
    initialize = function(clspec=getOption("mc.cores",2L),
                          clargs=list(),
                          seed=NULL,
                          stopClusterOnError=TRUE,
                          debug=FALSE,type=NULL) {
      clspec <- min(clspec,getOption("mc.cores",Inf))
      if (missing(type)) {
        self$cltype <- ifelse(.Platform$OS.type=="windows","PSOCK","FORK")
      } else {
        self$cltype <- type
      }
      self$clspec <- clspec
      self$clargs <- clargs
      self$seed <- seed
      self$stopClusterOnError <- stopClusterOnError
      self$debug <- debug
    },
    lapply=function(...) {
      if (is.null(private$cl))
        lapply(...)
      else
        parLapply(private$cl,...)
    },
    sapply=function(...) {
      if (is.null(private$cl))
        sapply(...)
      else
        parSapply(private$cl,...)
    },
    start = function(envir=parent.frame()) {
      if(!isTRUE(self$debug)) {
        if (!is.null(private$cl)) {
          warning("Trying to start the cluster twice.")
          return()
        }
        private$cl <- inject(makeCluster(self$clspec,self$cltype,
                                    !!!self$clargs))
        private$stopFlag <- self$stopClusterOnError
        if (!is.null(self$seed)) {
          clusterSetRNGStream(private$cl,self$seed)
          mc.reset.stream()
        }
        if (!is.null(envir))
          withr::defer(self$condStop(),envir=envir)
      }
    },
    flagStop=function() {
      private$stopFlag <- TRUE
    },
    isAlive=function() {
      if (is.null(private$cl)) return(FALSE)
      if (is(private$cl[[1]],"forknode")) {
        all(sapply(private$cl,\(x) parallelly::isConnectionValid(x$con)))
      } else {
        all(parallelly::isNodeAlive(private$cl))
      }
    },
    stop = function() {
      if (!is.null(private$cl)) {
        stopCluster(private$cl)
        private$cl <- NULL
        private$stopFlag <- FALSE
      }
    },
    condStop = function() {
      if (private$stopFlag) self$stop()
    }
  ),
  private=list(
    cl=NULL,
    stopFlag=FALSE
  )
)

isNodeAlive.SOCK0node <- function (x, ...) {
  parallelly::isConnectionValid(x$con)
}

