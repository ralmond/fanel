### Generic Functions ----
setGeneric("as.Panmat",function(obj,minocc=1L) {standardGeneric("as.Panmat")})

nsubj <- function(obj) {UseMethod("nsubj")}
setGeneric("nsubj")
setMethod("nsubj","NULL",function(obj) -Inf)
"nsubj<-" <- function(obj,value) {UseMethod("nsubj<-")}
setGeneric("nsubj<-")


bysubj <- function(obj) {UseMethod("bysubj")}
setGeneric("bysubj")
"bysubj<-" <- function(obj,value) {UseMethod("bysubj<-")}
setGeneric("bysubj<-")

isubj <- function(obj) {UseMethod("isubj")}
setGeneric("isubj")
"isubj<-" <- function(obj,value) {UseMethod("isubj<-")}
setGeneric("isubj<-")

nocc <- function(obj) {UseMethod("nocc")}
setGeneric("nocc")
setMethod("nocc","NULL",function(obj) -Inf)
"nocc<-" <- function(obj,value) {UseMethod("nocc<-")}
setGeneric("nocc<-")

byocc <- function(obj) {UseMethod("byocc")}
setGeneric("byocc")
"byocc<-" <- function(obj,value) {UseMethod("byocc<-")}
setGeneric("byocc<-")

maxocc <- function(obj) {UseMethod("maxocc")}
setGeneric("maxocc")
setMethod("maxocc","NULL",function(obj) -Inf)
"maxocc<-" <- function(obj,value) {UseMethod("maxocc<-")}
setGeneric("maxocc<-")

minocc <- function(obj) {UseMethod("minocc")}
setGeneric("minocc")
setMethod("minocc","NULL",function(obj) Inf)
"minocc<-" <- function(obj,value) {UseMethod("minocc<-")}
setGeneric("minocc<-")

nquad <- function(obj) {UseMethod("nquad")}
setGeneric("nquad")
"nquad<-" <- function(obj,value) {UseMethod("nquad<-")}
setGeneric("nquad<-")

getTime <- function(obj) {UseMethod("getTime")}
setGeneric("getTime")
"getTime<-" <- function(obj,value) {UseMethod("getTime<-")}
setGeneric("getTime<-")

getDT <- function(obj) {UseMethod("getDT")}
setGeneric("getDT")
"getDT<-" <- function(obj,value) {UseMethod("getDT<-")}
setGeneric("getDT<-")

qname <- function(obj) UseMethod("qname")
setGeneric("qname")
"qname<-" <- function(obj,value) UseMethod("qname<-")
setGeneric("qname<-")

dname <- function(obj) UseMethod("dname")
setGeneric("dname")
"dname<-" <- function(obj,value) UseMethod("dname<-")
setGeneric("dname<-")

wname <- function(obj) UseMethod("wname")
setGeneric("wname")
"wname<-" <- function(obj,value) UseMethod("wname<-")
setGeneric("wname<-")


as_longform <- function(x,...,n=nsubj(x),mxocc=maxocc(x),
                        mnocc=minocc(x),weightType="all",
                        name=deparse(substitute(x)),
                        includeTime=TRUE) {
  UseMethod("as_longform")
}
setGeneric("as_longform")

get_subj <- function(x,isub) {UseMethod("get_subj")}
setGeneric("get_subj")
"get_subj<-" <- function(x,isub,value) {UseMethod("get_subj<-")}
setGeneric("get_subj<-")

split_subj <- function(x) {
  lapply(1L:nsubj(x), \(isub) get_subj(x,isub))
}

add_subj <- function(x, xnew) {
  isub <- isubj(xnew)
  if (is.na(isub)) isub <- nsubj(x)+1L
  get_subj(x,isub) <- xnew
  x
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

mstep <- function(obj, data, ..., its=3,control=list()) {
  UseMethod("mstep")
}
setGeneric("mstep")

### FModel ----

FModel <- R6Class(
  classname="FModel",
  public=list(
    convergence=NA,
    lp=NA,
    name="FModel",
    wname="w",
    dnames="data",
    lprob = function(data,par=self$pvec) {
      stop("Lprob not implemented for ", class(self))
    },
    mstep=function(data, ..., its=3,control=list()) {
      control$maxits <- its
      result <- stats::optim(self$pvec,\(pv) self$lprob(data,pv),
                             ...,control)
      if (result$convergence > 1)
        warning("Convergence issues with ",
                self$toString(), "\n", result$message)
      self$lp <- result$value
      self$pvec <- result$par
      self$convergence <- result$convergence==0
      self
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
    qnames = function(value) {
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
"pvec<-.FModel" <- function(obj,value) {
  obj$pvec<- value
  obj
}
lprob.FModel <- function(obj,data,par=pvec(obj)) {
  obj$lprob(data,par)
}

mstep.FModel <- function(obj, data, ..., its=3,control=list()) {
  obj$mstep(data,...,its=its,control=list())
}

qname.FModel <- function(obj) obj$qnames
"qname<-.FModel" <- function(obj,value) {
  obj$qnames <- value
  obj
}

dname.FModel <- function(obj) obj$dnames
"dname<-.FModel" <- function(obj,value) {
  obj$dnames <- value
  obj
}

wname.FModel <- function(obj) obj$wname
"wname<-.FModel" <- function(obj,value) {
  obj$wname <- value
  obj
}

### ModelSet ----

ModelSet <- R6Class(
  "ModelSet",
  public=list(
    name="Model Set",
    iname=character(),
    models=list(),
    lp=NA,
    convergence=NA,
    index=1L,
    initialize=function(name,models,index=1L,
                        qname="theta",wname="w") {
      self$name <- name
      self$models <- models
      self$index <- as.Panmat(index)
      self$qnames <- qname
      self$wname <- wname
    },
    toString=function(...) {
      paste0("<",class(self)[1],": ",self$name,": ",
             nmodels(self), " >")
    },
    print=function(...) {
      print(self$toString(...),...)
    },
    mstep = function(data, ..., its=3, control=list()) {

      self$models <- lapply(self$split_m(data), \(pair) {
        pair[[1]]$mstep(pair[[2]],...,its=its,control=control)
      })
      self$lp <- 0
      self$convergence <- logical()
      for (imod in 1:nmodels(self)) {
        self$lp <- self$lp + self$models[[imod]]$lp
        self$convergence <- c(self$convergence,
                              self$models[[imod]]$convergence)
      }
      self
    },
    prepData = function(data) {
      data[!is.na(data[[self$iname]]),]
    },
    split_m = function(data) {
      data <- self$prepData(data)
      mods <- na.omit(unique(data[[self$iname]]))
      lapply(mods,
             \(im) {
               list(self$models[[im]],
                    dplyr::filter(data,.data[[self$iname]]==im))
      })
    }
  ),
  active=list(
    qnames = function(value) {
      if (length(self$models)==0L) {
        warn("No models defined.")
        return(character())
      }
      if (missing(value)) return(self$models[[1]]$qnames)
      lapply(self$models, \(m) m$qnames <- value)
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

mstep.ModelSet <- function(obj, data, ..., its=3,control=list()) {
  obj$mstep(data,...,its=its,control=control)
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

mstep.ModelSet <- function(obj, data, ..., its=3,control=list()) {
  obj$mstep(data, ..., its=its, control=control)
}

as_longform.ModelSet <- function(x,...,n=nsubj(x),mxocc=maxocc(x),
                   mnocc=minocc(x)) {
  as_longform(x$index,n=n,mxocc=mxocc,mnocc=mnocc,
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

probInit <- function(model, isubj, thetas, covar=NULL) {
  UseMethod("probInit")
}
setGeneric("probInit")

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

qname.ModelSet <- function(obj) {
  obj$qnames
}
"qname<-.ModelSet" <- function(obj,value) {
  obj$qnames <- value
  obj
}

dname.ModelSet <- function(obj) obj$dnames
"dname<-.ModelSet" <- function(obj,value) {
  obj$dnames <- value
  obj
}

wname.ModelSet <- function(obj) obj$wname
"wname<-.ModelSet" <- function(obj,value) {
  obj$wname <- value
  obj
}


getTime.ModelSet <- function(obj) {
  obj$times
}
"getTime<-.ModelSet" <- function(obj,value) {
  if (is.null(obj$times)) {
    stop("Times are not settable for object of class",
         class(obj),".")
  }
  obj$times <- value
  obj
}


getDT.ModelSet <- function(obj) {
  obj$dt
}
"getDT<-.ModelSet" <- function(obj,value) {
  if (is.null(obj$dt)) {
    stop("Delta times are not settable for object of class",
         class(obj),".")
  }
  obj$dt <- value
  obj
}

