
setGeneric("as.Panmat",function(obj,minocc=1L) standardGeneric("as.Panmat"))

setGeneric("nsubj",function(obj) standardGeneric("nsubj"))
setGeneric("nsubj<-",function(obj,value) standardGeneric("nsubj<-"))

setGeneric("isubj",function(obj) standardGeneric("isubj"))
setGeneric("isubj<-",function(obj,value) standardGeneric("isubj<-"))

setGeneric("nocc",function(obj) standardGeneric("nocc"))
setGeneric("nocc<-",function(obj,value) standardGeneric("nocc<-"))

setGeneric("maxocc",function(obj) standardGeneric("maxocc"))
setGeneric("maxocc<-",function(obj,value) standardGeneric("maxocc<-"))

setGeneric("minocc",function(obj) standardGeneric("minocc"))
setGeneric("minocc<-",function(obj,value) standardGeneric("minocc<-"))

setGeneric("nquad",function(obj) standardGeneric("nquad"))
setGeneric("nquad<-",function(obj,value) standardGeneric("nquad<-"))

setGeneric("as_longform",function(x,n=nsubj(x),maxocc=maxocc(x),
                                  minocc=minocc(x),weightType="all",
                                  name=deparse(substitute(x))) {
  standardGeneric("as_longform")
})

setGeneric("get_subj",function(x,subj) standardGeneric("get_subj"))
setGeneric("get_subj<-",function(x,subj,value) standardGeneric("get_subj<-"))

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


setGeneric("pvec",function(obj) standardGeneric("pvec"))
setGeneric("pvec<-",function(obj,value) standardGeneric("pvec<-"))


setGeneric("lprob",
           function(obj,data,par=pvec(obj)) {
             standardGeneric("lprob")
           })

setGeneric("mstep",
           function(obj, data, its=3,control=list(),
                    workers=Workers$new()) {
             standardGeneric("mstep")
           })

setMethod("mstep","ANY", function(obj, data, its=3, control=list(),
                                      workers=Workers$new()) {
  control$maxits <- its
  result <- stats::optim(pvec(obj),\(pv) lprob(obj,pv,data))
  if (result$convergence > 1)
    warning("Convergence issues with ",
            toString(obj), "\n", result$message)
  obj$lp <- result$value
  obj$pvec <- result$par
  obj$convergence <- result$convergence
  list(name=obj$name,result)
})


FModel <- R6Class(
  classname="FModel",
  public=list(
    covergence=NA,
    lp=NA,
    name="FModel",
    wname="w",
    lprob = function(par=self$pvec,data) {
      stop("Lprob not implemented for ", class(self))
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

setMethod("pvec","FModel",function(obj) obj$pvec)
setMethod("pvec<-","FModel",function(obj,value) obj$pvec<- value)
setMethod("lprob","FModel",
           function(obj,data,par=pvec(obj)) {
             obj$lprob(par,data)
           })

ModelSet <- R6Class(
  "ModelSet",
  public=list(
    name="Model Set",
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
    mstep = function(data, its=3, control=list(), workers=Workers$new()) {
      stop("No mstep function implemented for collection ",nameOfClass(self))
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

setMethod("mstep","ModelSet",
          function(obj, data, its=3,control=list(),
                   workers=Workers$new()) {
            obj$mstep(data)
          })

setMethod("nsubj","ModelSet", function(obj) nsubj(obj$index))
setMethod("nsubj<-","ModelSet", function(obj,value) {
  nsubj(obj$index) <-as.integer(value)
  obj
})


setMethod("nocc","ModelSet", function(obj) nocc(obj$index))
setMethod("nocc<-","ModelSet", function(obj,value) {
  nocc(obj$index) <- as.integer(value)
  obj
})

setMethod("maxocc","ModelSet", function(obj) maxocc(obj$index))
setMethod("maxocc<-","ModelSet", function(obj,value) {
  maxocc(obj$index) <- value
  obj
})


setMethod("minocc","ModelSet", function(obj) minocc(obj$index))
setMethod("minocc<-","ModelSet", function(obj,value) {
  minocc(obj$index) <- as.integer(value)
  obj
})

setMethod("mstep","ModelSet",
          function(obj, data, its=3,control=list(),
                   workers=Workers$new()) {
            obj$mstep(data)
          })
