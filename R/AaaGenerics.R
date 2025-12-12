
setGeneric("as.Panmat",function(obj,minocc=1L) standardGeneric("as.Panmat"))

nsubj <- function(obj) {UseMethod("nsubj")}
setGeneric("nsubj")
"nsubj<-" <- function(obj,value) {UseMethod("nsubj<-")}
setGeneric("nsubj<-")

isubj <- function(obj) {UseMethod("isubj")}
setGeneric("isubj")
"isubj<-" <- function(obj,value) {UseMethod("isubj<-")}
setGeneric("isubj<-")

nocc <- function(obj) {UseMethod("nocc")}
setGeneric("nocc")
"nocc<-" <- function(obj,value) {UseMethod("nocc<-")}
setGeneric("nocc<-")

maxocc <- function(obj) {UseMethod("maxocc")}
setGeneric("maxocc")
"maxocc<-" <- function(obj,value) {UseMethod("maxocc<-")}
setGeneric("maxocc<-")

minocc <- function(obj) {UseMethod("minocc")}
setGeneric("minocc")
"minocc<-" <- function(obj,value) {UseMethod("minocc<-")}
setGeneric("minocc<-")

nquad <- function(obj) {UseMethod("nquad")}
setGeneric("nquad",function(obj) standardGeneric("nquad"))
"nquad<-" <- function(obj,value) {UseMethod("nquad<-")}
setGeneric("nquad<-")


as_longform <- function(x,...,n=nsubj(x),maxocc=maxocc(x),
                        minocc=minocc(x),weightType="all",
                        name=deparse(substitute(x))) {
  UseMethod("as_longform")
}
setGeneric("as_longform")

get_subj <- function(x,isubj) {UseMethod("get_Subj")}
setGeneric("get_subj")
"get_subj<-" <- function(x,isubj,value) {UseMethod("get_Subj<-")}
setGeneric("get_subj<-")

split_subj <- function(x) {
  lapply(1L:nsubj(x), \(isubj) get_subj(x,isubj))
}

add_subj <- function(x, xnew) {
  isubj <- isubj(xnew)
  if (is.na(isubj)) isubj <- nsubj(x)+1L
  get_subj(x,isubj) <- xnew
}

bind_subj <- function(xlist) {
  xlist[order(purrr::map_int(xlist,"isubj"))] |>
  purrr::reduce(add_subj)
}


pvec <- function(obj) {UseMethod("pvec")}
setGeneric("pvec")
"pvec<-" <- function(obj,value) {UseMethod("pvec<-")}
setGeneric("pvec<-")


lprob <- function(obj,data,par=pvec(obj)) {
  UseMethod("lprob")
}
setGeneric("lprob")

mstep <- function(obj, data, ..., its=3,control=list(),
                  workers=Workers$new()) {
  UseMethod("mstep")
}
setGeneric("mstep")


FModel <- R6Class(
  classname="FModel",
  public=list(
    covergence=NA,
    lp=NA,
    name="FModel",
    wname="w",
    lprob = function(data,par=self$pvec) {
      stop("Lprob not implemented for ", class(self))
    },
    mstep=function(data, ..., its=3,control=list()) {
      control$maxits <- its
      result <- stats::optim(self$pvec,\(pv) obj$lprob(data,pv))
      if (result$convergence > 1)
        warning("Convergence issues with ",
                self$toString(), "\n", result$message)
      self$lp <- result$value
      self$pvec <- result$par
      self$convergence <- result$convergence
      list(name=self$name,result)
    },
    print=function(...) {
      print(self$toString(...),...)
    },
    toString=function(...) {
      paste0("<",self$name,">")
    }
  ),
  private=list(
    thetaNames=character()
  ),
  active=list(
    pvec = function(value) {
      stop("Pvec active field not implemented for",class(self))
    },
    tnames = function(value) {
      if (missing(value)) return(private$thetaNames)
      if (length(private$thetaNames) > 0L &&
          length(private$thetaNames) != length(value)) {
        stop("Theta names must have length",length(private$thetaNames))
      }
      private$thetaNames <- value
    }
  )
)

setOldClass("FModel")

pvec.FModel <- function(obj) {obj$pvec}
"pvec<-.FModel" <- function(obj,value) {obj$pvec<- value}
lprob.FModel <- function(obj,data,par=pvec(obj)) {
  obj$lprob(data,par)
}

mstep.FModel <- function(obj, data, ..., its=3,control=list()) {
  obj$mstep(data,...,its=its,control=list())
}

ModelSet <- R6Class(
  "ModelSet",
  public=list(
    name="Model Set",
    iname=character(),
    models=list(),
    index=1L,
    initialize=function(name,models,index=1L,
                        tname="theta",wname="w") {
      self$name <- name
      self$models <- models
      self$index <- as.Panmat(index)
      self$tnames <- tname
      self$wname <- wname
    },
    toString=function(...) {
      paste0("<",nameOfClass(self),": ",self$name,": ",
             self$nsubjects, " >")
    },
    print=function(...) {
      print(self$toString(...),...)
    },
    mstep = function(data, ..., its=3, control=list(),
                     workers=Workers$new(nmodels(self))) {
      workers.start()
      result <- workers.lapply(self$split_m(data), \(pair) {
        pair[[1]]$mstep(pair[[2]])
      })
      workers$stopFlag()
      result
    },
    prepData = identity,
    split_m = function(data) {
      data <- self$prepData(data)
      lapply(unique(data[[self$iname]]),\(im) {
        list(self$models[[im]],dplyr::filter(data,.data[[self$iname]]==im))
      })
    }
  ),
  active=list(
    tnames = function(value) {
      if (length(self$models)==0L) {
        warn("No models defined.")
        return(character())
      }
      if (missing(value)) return(self$models[[1]]$tnames)
      lapply(self$models, \(m) m$tnames <- value)
    },
    wname = function(value) {
      if (length(self$models)==0L) {
        warn("No models defined.")
        return(character())
      }
      if (missing(value)) return(self$models[[1]]$wname)
      lapply(self$models, \(m) m$wname <- value)
    },
    dnames = function(value) {
      if (length(self$models)==0L) {
        warn("No models defined.")
        return(character())
      }
      if (missing(value)) return(self$models[[1]]$dnames)
      lapply(self$models, \(m) m$dnames <- value)
    }
  )
)

setOldClass("ModelSet")

mstep.ModelSet <- function(obj, data, ..., its=3,control=list(),
                   workers=Workers$new()) {
  obj$mstep(data,...,its=its,control=control, workers=workers)
}

nsubj.ModelSet <- function(obj) {nsubj(obj$index)}
"nsubj<-.ModelSet" <- function(obj,value) {
  nsubj(obj$index) <-as.integer(value)
  obj
}


nocc.ModelSet <- function(obj) {nocc(obj$index)}
"nocc<-.ModelSet" <- function(obj,value) {
  nocc(obj$index) <- as.integer(value)
  obj
}

maxocc.ModelSet <- function(obj) {maxocc(obj$index)}
"maxocc<-.ModelSet" <- function(obj,value) {
  maxocc(obj$index) <- value
  obj
}


"minocc.ModelSet" <- function(obj) {minocc(obj$index)}
"minocc<-.ModelSet" <- function(obj,value) {
  minocc(obj$index) <- as.integer(value)
  obj
}

mstep.ModelSet <- function(obj, data, ..., its=3,control=list(),
                           workers=Workers$new()) {
  obj$mstep(data, ..., its=its, control=control, workers=workers)
}

as_longform.ModelSet <- function(x,...,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,name=deparse(substitute(x))) {
  as_longform(x$index,n=n,maxocc=maxocc,minocc=minocc,
              name=x$iname)
}

nmodels <- function(obj) {UseMethod("nmodels")}
setGeneric("nmodels")

nmodels.ModelSet <- function(obj) {
  length(obj$models)
}


### Generic Model Set Functions

drawInitial <- function(model, isubj, npart, covar=NULL) {
  UseMethod("drawInitial")
}
setGeneric("drawInitial")

ProbInit <- function(model, isubj, thetas, covar=NULL) {
  UseMethod("ProbInit")
}
setGeneric("ProbInit")

drawData <- function(model,isubj,iocc,theta,covar=NULL) {
  UseMethod("drawData")
}
setGeneric("drawData")

evalEvidence <- function(model, isubj, iocc, theta, data,
                                    covar=NULL) {
  UseMethod("evalEvidence")
}
setGeneric("evalEvidence")



drawGrowth <- function(model, isubj, iocc, theta, covar=NULL) {
  UseMethod("drawGrowth")
}
setGeneric("drawGrowth")

advanceWeights <- function(model, isubj, iocc, lweights,
                                      covar=NULL) {
  UseMethod("advanceWeights")
}
setGeneric("advanceWeights")


retreatWeights <- function(model, isubj, iocc, rweights,
                                      covar=NULL) {
  UseMethod("retreatWeights")
}
setGeneric("retreatWeights")

